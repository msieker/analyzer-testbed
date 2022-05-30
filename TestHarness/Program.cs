using System.Collections.Immutable;
using Microsoft.Build.Locator;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.Diagnostics;
using Microsoft.CodeAnalysis.MSBuild;
using TestHarness;

/*
 * This 23w
 */

MSBuildLocator.RegisterDefaults();

using var workspace = MSBuildWorkspace.Create();

var project = await workspace.OpenProjectAsync(@"c:\icg\CodeAnalysisTest\CodeAnalysisTest\CodeAnalysisTest.csproj");

var compilation = await project.GetCompilationAsync();

var result = await compilation!.WithAnalyzers(ImmutableArray.Create<DiagnosticAnalyzer>(new FakerAnalyzer(), new ExporterAnalyzer())).GetAllDiagnosticsAsync();

foreach (var d in result.Where(d=>!(d.IsSuppressed || d.Severity == DiagnosticSeverity.Hidden)))
{
    Console.WriteLine(d.ToString());
}

//var driver = CSharpGeneratorDriver.Create(new AutoExportGenerator());
//driver.RunGenerators(compilation!);

//Console.WriteLine(driver.GetRunResult().Dump());