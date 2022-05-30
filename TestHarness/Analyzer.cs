using System.Collections.Immutable;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Diagnostics;

namespace TestHarness;
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
        "CAT002",
        "Exporter have a constructor that takes an entity",
        "The exporter {0} is missing a public constructor which takes a {1} instance",
        "CodeAnalysisTest",
        DiagnosticSeverity.Error,
        true
    );
}

[DiagnosticAnalyzer(LanguageNames.CSharp)]
public class FakerAnalyzer : DiagnosticAnalyzer
{
    public override ImmutableArray<DiagnosticDescriptor> SupportedDiagnostics { get; } =
        ImmutableArray.Create(AnalyzerDescriptors.FakerMustConfigureAllEntityProperties);

    public override void Initialize(AnalysisContext context)
    {
        context.ConfigureGeneratedCodeAnalysis(GeneratedCodeAnalysisFlags.Analyze);
        context.EnableConcurrentExecution();
        context.RegisterSyntaxNodeAction(AnalyzeConstructors, SyntaxKind.ConstructorDeclaration);
    }

    private static void AnalyzeConstructors(SyntaxNodeAnalysisContext context)
    {
        var containingClass = context.ContainingSymbol?.ContainingType; // Grab the class containing this constructor

        if (containingClass?.BaseType == null) return;  // If the class doesn't inherit from anything, its of no interest

        var faker = context.Compilation.GetTypeByMetadataName("Bogus.Faker`1");     // Grab the Faker<T> open generic interface

        /*
         * Checks that the class being inspected inherits from Faker<T>.
         * The SymbolEqualityComparer is needed to compare bits of the compiler model (and is a warning to do it any other way)
         * The ConstructedFrom bit is because BaseType itself is the closed generic type, and not the open one we're looking for.
         */

        if (!SymbolEqualityComparer.Default.Equals(containingClass.BaseType.ConstructedFrom, faker)) return;

        var baseEntity = containingClass.BaseType.TypeArguments[0];

        foreach(var d in AnalyzeFakerInit((context.Node as ConstructorDeclarationSyntax)!, context.SemanticModel, (context.ContainingSymbol as IMethodSymbol)!, baseEntity))
        {
            context.ReportDiagnostic(d);
        }
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

    /*
     * Nothing involving the parse tree here (Which might not even exist if the entity is in another assembly).
     * We just ask the compiler for the public properties on the type. Read only properties are ignored since
     * Bogus doesn't need to touch them.
     */
    private static IEnumerable<IPropertySymbol> GetEntityProperties(ITypeSymbol entity)=>
        entity.GetMembers().Where(m => m.DeclaredAccessibility == Accessibility.Public && m.Kind == SymbolKind.Property)
            .OfType<IPropertySymbol>()
            .Where(p=>p.IsReadOnly == false);

    private static IEnumerable<Diagnostic> AnalyzeFakerInit(ConstructorDeclarationSyntax ctorSyntax, SemanticModel model, IMethodSymbol ctor, ITypeSymbol entity)
    {
        var ruleForProperties = GetRuleForProperties(model, ctor).ToList();
        var entityProperties = GetEntityProperties(entity).ToList();

        var unassigned = entityProperties.Where(ep => !ruleForProperties.Any(rf => SymbolEqualityComparer.Default.Equals(ep, rf)));
        foreach (var up in unassigned)
        {
            yield return Diagnostic.Create(
                AnalyzerDescriptors.FakerMustConfigureAllEntityProperties,
                ctorSyntax.Identifier.GetLocation(),
                entity.Name, up.Name
                );
        }
    }
}

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
        var exporter = context.Compilation.GetTypeByMetadataName("CodeAnalysisTest.Exporter`1");
        if (!SymbolEqualityComparer.Default.Equals(typeSymbol.BaseType.ConstructedFrom, exporter)) return;
        Console.WriteLine("Exporter: " + typeSymbol.Name);

        var baseEntity = typeSymbol.BaseType.TypeArguments[0];

        CompareExporterAndEntity(context.SemanticModel, typeSymbol, baseEntity);
    }

    private static IEnumerable<IPropertySymbol> GetEntityProperties(ITypeSymbol entity)
    {
        return entity.GetMembers().Where(m => m.DeclaredAccessibility == Accessibility.Public && m.Kind == SymbolKind.Property)
            .OfType<IPropertySymbol>()
            .Where(p => p.IsReadOnly == false);
    }

    private static IEnumerable<IMethodSymbol> GetConstructorsWithEntity(INamedTypeSymbol exporter, ITypeSymbol entity)
        => exporter.Constructors.Where(c => c.DeclaredAccessibility == Accessibility.Public && c.Parameters.Any(p => SymbolEqualityComparer.Default.Equals(p.Type, entity)));

    private static void CompareExporterAndEntity(SemanticModel model, INamedTypeSymbol exporter, ITypeSymbol entity)
    {
        var constructors = GetConstructorsWithEntity(exporter, entity);

    }
}