using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp.Syntax;

namespace TestHarness;

[Generator]
internal class AutoExportGenerator : IIncrementalGenerator
{
    public void Initialize(IncrementalGeneratorInitializationContext context)
    {
        context.SyntaxProvider.CreateSyntaxProvider(
                static (s, _) => IsClassWithBaseClass(s),
                static (c, _) => IsMarkedAsAutoExport(c))
            .Where(static (c) => c is not null);
    }

    private static bool IsClassWithBaseClass(SyntaxNode node) => node is ClassDeclarationSyntax { BaseList: { } };

    private static ClassDeclarationSyntax? IsMarkedAsAutoExport(GeneratorSyntaxContext context)
    {
        return null;
    }
}

