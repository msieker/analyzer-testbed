using System.Collections.Immutable;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.Diagnostics;

namespace CodeAnalysisTest.Analyzers;

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

        context.RegisterSymbolAction(AnalyzeNamedType, SymbolKind.NamedType);
    }

    private static void AnalyzeNamedType(SymbolAnalysisContext context)
    {
        
    }
}
