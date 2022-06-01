using System.Collections.Immutable;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Diagnostics;

namespace TestHarness;

/*
 * This seems to be the "correct" way of declaring diagnostic messages that can be emitted by analyzers.
 */
public static class AnalyzerDescriptors
{
    public static readonly DiagnosticDescriptor FakerMustConfigureAllEntityProperties = new(
        "CAT001",
        "Faker must have RuleFor for all entity properties",
        "The faker for {0} is missing a RuleFor for property {1}",
        "CodeAnalysisTest",
        DiagnosticSeverity.Warning,
        true
    );

    public static readonly DiagnosticDescriptor ExporterMustHaveAllEntityProperties = new(
        "CAT002",
        "Exporter must have properties matching entity properties, or use Ignore",
        "The exporter for {0} is missing a property {1}",
        "CodeAnalysisTest",
        DiagnosticSeverity.Warning,
        true
    );

    public static readonly DiagnosticDescriptor ExporterMustHaveConstructorTakingEntity = new(
        "CAT003",
        "Exporter have a constructor that takes an entity",
        "The exporter {0} is missing a public constructor which takes a {1} instance",
        "CodeAnalysisTest",
        DiagnosticSeverity.Error,
        true
    );
}

/*
 * Some helper methods that have been used in more than one place
 */
public static class Helpers
{
    /*
     * Gets all public properties on a type.
     */
    public static IEnumerable<IPropertySymbol> GetTypeProperties(this ITypeSymbol type) =>
        type.GetMembers().Where(static m => m.DeclaredAccessibility == Accessibility.Public && m.Kind == SymbolKind.Property)
            .OfType<IPropertySymbol>();


    /*
     * Gets all read-write properties on a type.
     */
    public static IEnumerable<IPropertySymbol> GetReadOnlyTypeProperties(this ITypeSymbol type)
        => GetTypeProperties(type).Where(static p => p.IsReadOnly == false);

    /*
     * Gets all public constructors on a type that have at least one parameter of a specified type.
     */
    public static IEnumerable<IMethodSymbol> GetConstructorsWithAParameterOfType(this INamedTypeSymbol classDecl, ITypeSymbol parameterType) =>
        classDecl.Constructors.Where(c => c.DeclaredAccessibility == Accessibility.Public && c.Parameters.Any(p => SymbolEqualityComparer.Default.Equals(p.Type, parameterType)));
}

[DiagnosticAnalyzer(LanguageNames.CSharp)]
public class FakerAnalyzer : DiagnosticAnalyzer
{
    /*
     * This is declared on DiagnosticAnalyzer. This contains a list of all possible
     * messages this analyzer can return.
     */
    public override ImmutableArray<DiagnosticDescriptor> SupportedDiagnostics { get; } =
        ImmutableArray.Create(AnalyzerDescriptors.FakerMustConfigureAllEntityProperties);

    public override void Initialize(AnalysisContext context)
    {
        context.ConfigureGeneratedCodeAnalysis(GeneratedCodeAnalysisFlags.Analyze);

        /*
         * Comment this out if you're wanting to debug an analyzer. With this on,
         * the compiler will make multiple concurrent calls to the actions registered below.
         * However, for actual use, it should be here, there's even an analyzer that will complain
         * if this line isn't present.
         */
        context.EnableConcurrentExecution();

        /*
         * Registers callbacks from the compiler when it hits a class declaration when doing
         * semantic analysis. Since this analyzer needs both the parse tree and the semantic model
         * of the compiled code, this is the best place to hook in.
         */
        context.RegisterSyntaxNodeAction(AnalyzeClassDeclarations, SyntaxKind.ClassDeclaration);
    }

    private static void AnalyzeClassDeclarations(SyntaxNodeAnalysisContext context)
    {
        var containingClass = context.ContainingSymbol as INamedTypeSymbol;

        if (containingClass?.BaseType == null) return;  // If the class doesn't inherit from anything, its of no interest

        var faker = context.Compilation.GetTypeByMetadataName("Bogus.Faker`1");     // Grab the Faker<T> open generic interface
        
        if(faker == null) return; // Can't find the faker class. Must not be in this project.

        /*
         * Checks that the class being inspected inherits from Faker<T>.
         * The SymbolEqualityComparer is needed to compare bits of the compiler model (and is a warning to do it any other way)
         * The ConstructedFrom bit is because BaseType itself is the closed generic type, and not the open one we're looking for.
         */
        if (!SymbolEqualityComparer.Default.Equals(containingClass.BaseType.ConstructedFrom, faker)) return;

        /*
         * Pull out the type argument from the base class. Since we're explicitly looking for a class with a
         * generic base class, this type argument should be there if we got this far.
         */
        var baseEntity = containingClass.BaseType.TypeArguments[0];

        AnalyzeFaker(context, containingClass, baseEntity);
    }

    private static IEnumerable<IPropertySymbol> GetRuleForProperties(SemanticModel model, IMethodSymbol ctor)
    {
        var ctorLocation = ctor.Locations.FirstOrDefault();

        /*
         * This bit delves into the parse tree for the constructor to figure out what
         * properties RuleFor is called on. To get what some of these syntax tokens are called
         * you can use the View -> Other Windows -> Syntax Visualizer in Visual Studio. This will
         * let you click around in the text editor and find out what the parser calls various bits.
         *
         * In this case, we're looking for an InvocationExpression (a combination of an identifier and argument list)
         * with an IdentifierName of "RuleFor".
         */
        var node = ctorLocation?.SourceTree?.GetRoot().FindNode(ctorLocation.SourceSpan);
        var ruleForStatements = from n in node?.DescendantNodes().OfType<InvocationExpressionSyntax>()
                                let id = n.Expression as IdentifierNameSyntax
                                where id != null
                                where id.Identifier.ValueText == "RuleFor"
                                select n;

        /*
         * Once we have a list of RuleFor statements in the constructor, pluck the first MemberAccessExpression
         * (Which might be shown in the syntax tree as a SimpleMemberAccessExpression), which for how RuleFor is used
         * will be in the property lambda expression in the first parameter. Once we have that bit of the parse tree,
         * we can ask the semantic model "Hey, what does this point to?" and get back the property on the entity class.
         */
        foreach (var s in ruleForStatements)
        {
            var entityMember = s.DescendantNodes().OfType<MemberAccessExpressionSyntax>().First();
            if (model.GetSymbolInfo(entityMember).Symbol is not IPropertySymbol property) continue;
            yield return property;
        }
    }

    private static void AnalyzeFaker(SyntaxNodeAnalysisContext context, INamedTypeSymbol fakerClass, ITypeSymbol entity)
    {
        // The faker class might have multiple constructors (unlikely), check each one
        foreach (var ctor in fakerClass.Constructors)
        {
            // Get all of the entity properties referenced by RuleFor statements
            var ruleForProperties = GetRuleForProperties(context.SemanticModel, ctor).ToList();
            // Get all of the read only public properties on the entity
            var entityProperties = entity.GetReadOnlyTypeProperties().ToList();

            /*
             * Do a simple check that all of the entity properties have RuleFor statements
             * Probably better to do something a bit smarter than a O(n*m) check, but its simple to read.
             * And n and m will probably never be bigger than a few tens.
             */
            var unassigned = entityProperties.Where(ep => !ruleForProperties.Any(rf => SymbolEqualityComparer.Default.Equals(ep, rf)));
            foreach (var up in unassigned)
            {
                // Figure out where the ctor is so we can point the warning at it.
                var ctorLocation = ctor.Locations.First();

                // Emit the warning
                context.ReportDiagnostic(
                    Diagnostic.Create(
                        AnalyzerDescriptors.FakerMustConfigureAllEntityProperties,
                        ctorLocation,
                        entity.Name, up.Name
                    )
                );
            }
        }
    }
}

internal record AssignmentInfo(ISymbol Left, ISymbol Right);

[DiagnosticAnalyzer(LanguageNames.CSharp)]
public class ExporterAnalyzer : DiagnosticAnalyzer
{
    public override ImmutableArray<DiagnosticDescriptor> SupportedDiagnostics { get; } =
        ImmutableArray.Create(
            AnalyzerDescriptors.ExporterMustHaveAllEntityProperties,
            AnalyzerDescriptors.ExporterMustHaveConstructorTakingEntity
            );

    public override void Initialize(AnalysisContext context)
    {
        context.ConfigureGeneratedCodeAnalysis(GeneratedCodeAnalysisFlags.Analyze);
        context.EnableConcurrentExecution();
        context.RegisterSyntaxNodeAction(AnalyzeClasses, SyntaxKind.ClassDeclaration);
    }

    private static void AnalyzeClasses(SyntaxNodeAnalysisContext context)
    {
        var typeSymbol = context.ContainingSymbol as INamedTypeSymbol;
        if (typeSymbol?.BaseType == null || typeSymbol.IsAbstract) return;

        /*
         * This is kind of hackish, and I need to figure out the best way of grabbing a reference
         * to the type we're looking for when used in a non-testing setup. If the exporter classes
         * are in a separate assembly in a nuget, then it'll have a stable name.
         */
        var exporter = context.Compilation.GetTypeByMetadataName("CodeAnalysisTest.Exporter`1");
        if (!SymbolEqualityComparer.Default.Equals(typeSymbol.BaseType.ConstructedFrom, exporter)) return;
        Console.WriteLine("Exporter: " + typeSymbol.Name);

        var baseEntity = typeSymbol.BaseType.TypeArguments[0];

        CompareExporterAndEntity(context, typeSymbol, baseEntity);
    }

    private static IEnumerable<AssignmentInfo> GetEntityAssignmentsInMethod(SyntaxNodeAnalysisContext context, IMethodSymbol method, ITypeSymbol entity)
    {
        var entityProps = entity.GetTypeProperties().ToList();

        // Might be looking at a partial method, this gets all places a method is defined.
        foreach (var rootSyntax in method.DeclaringSyntaxReferences)
        {
            // Look for assignments in the method. 
            var assignments = rootSyntax.GetSyntax().DescendantNodes().OfType<AssignmentExpressionSyntax>();
            foreach (var a in assignments)
            {
                var left = context.SemanticModel.GetSymbolInfo(a.Left).Symbol;

                /*
                 * This looks through the right side of the assignment for any member accesses. As an example, something like
                 * m.Property.ToString() would have syntax for both the full expression, and the m.Property. To filter out
                 * nested expressions like that, look for those expressions that just reference properties on the entity.
                 *
                 * TODO: This will have to change when dealing with flattening
                 */
                var accesses = from mae in a.Right.DescendantNodesAndSelf().OfType<MemberAccessExpressionSyntax>()
                               let symbol = context.SemanticModel.GetSymbolInfo(mae).Symbol
                               where symbol != null && entityProps.Any(ep => SymbolEqualityComparer.Default.Equals(ep, symbol))
                               select symbol;

                /*
                 * It's possible for there to be multiple property accesses on the RHS. Something like $"{m.Prop1}: {m.Prop2}".
                 * For the sake of this analyzer, we'll count all of them.
                 */
                foreach (var r in accesses)
                {
                    if (r == null)
                    {
                        Console.WriteLine($"Unknown syntax: {a.Right.Dump(new DumpOptions { MaxLevel = 1 })}");
                        continue;
                    }
                    yield return new AssignmentInfo(left!, r);
                }
            }
        }
    }

    public static IEnumerable<ISymbol> FindIgnoredFieldsInMethod(SyntaxNodeAnalysisContext context, IMethodSymbol method, ITypeSymbol entity)
    {
        /*
         * This is basically a retread of the RuleFor() analyzer, except looking for Ignore() calls instead.
         */
        foreach (var rootSyntax in method.DeclaringSyntaxReferences)
        {
            var ignoreStatements = from n in rootSyntax.GetSyntax().DescendantNodes().OfType<InvocationExpressionSyntax>()
                let id = n.Expression as IdentifierNameSyntax
                where id != null
                where id.Identifier.ValueText == "Ignore"
                select n;
            
            foreach (var s in ignoreStatements)
            {
                var entityMember = s.DescendantNodes().OfType<MemberAccessExpressionSyntax>().First();
                if (context.SemanticModel.GetSymbolInfo(entityMember).Symbol is not IPropertySymbol property) continue;
                yield return property;
            }
        }
    }

    private static void CompareExporterAndEntity(SyntaxNodeAnalysisContext context, INamedTypeSymbol exporter, ITypeSymbol entity)
    {
        var constructors = exporter.GetConstructorsWithAParameterOfType(entity).ToList();

        if (!constructors.Any())
        {
            /*
             * Part of the exporter contract is that exporters have a public constructor that
             * take the entity to export as a parameter. Throw an error if there isn't one.
             */
            context.ReportDiagnostic(
                Diagnostic.Create(
                    AnalyzerDescriptors.ExporterMustHaveConstructorTakingEntity,
                    exporter.Locations.First(l => l.IsInSource),
                    exporter.Name, entity.Name));
            return;
        }
        var entityProperties = entity.GetReadOnlyTypeProperties().ToList();
        
        foreach (var ctor in constructors)
        {
            var assignments = GetEntityAssignmentsInMethod(context, ctor, entity).ToList();
            var ignored = FindIgnoredFieldsInMethod(context, ctor, entity).ToList();
            foreach (var a in assignments)
            {
                Console.WriteLine($"{a.Left.ToDisplayString()} <- {a.Right.ToDisplayString()}");
            }

            foreach (var i in ignored)
            {
                Console.WriteLine($"Ignored: {i.ToDisplayString()}");
            }

            var foundProperties = assignments.Select(a => a.Right).Concat(ignored);

            var missingAssignments = entityProperties.Where(ep => !foundProperties.Any(p => SymbolEqualityComparer.Default.Equals(ep, p)));

            foreach (var a in missingAssignments)
            {
                // Figure out where the ctor is so we can point the warning at it.
                var ctorLocation = ctor.Locations.First();

                // Emit the warning
                context.ReportDiagnostic(
                    Diagnostic.Create(
                        AnalyzerDescriptors.ExporterMustHaveAllEntityProperties,
                        ctorLocation,
                        entity.Name, a.Name
                    )
                );
            }
        }
    }
}