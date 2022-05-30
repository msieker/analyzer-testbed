using System.Collections.Immutable;
using Microsoft.Build.Locator;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.Diagnostics;
using Microsoft.CodeAnalysis.MSBuild;
using TestHarness;

/*
 * Using MSBuildLocator is the recommended way of interacting with build projects.
 * This is done instead of giving some code directly to Roslyn (which is how most examples are done)
 * so I don't have to deal with figuring out how to include Bogus or any other nuget packages that
 * might get pulled in. Plus the source files can still be edited in VS instead of as a string in a
 * source file.
 */
MSBuildLocator.RegisterDefaults();

using var workspace = MSBuildWorkspace.Create();

/*
 * One downside to this: If you want to use both dotnet run and the VS debugger on this project, this needs to be
 * an absolute path, since there will be different working directories for the two run methods. So change this
 * path to match your own setup.
 */
var project = await workspace.OpenProjectAsync(@"c:\icg\CodeAnalysisTest\CodeAnalysisTest\CodeAnalysisTest.csproj");

var compilation = await project.GetCompilationAsync();

/*
 * Attaches the analyzers to the compiled code and gets their output. In this case, compilation can only be null if
 * whatever project GetCompilationAsync was called on doesn't support compilation. As we're explicitly giving it a
 * csproj file, we know better than the compiler about if it'll be null.
 *
 * Any new analyzers get put here.
 */
var result = await compilation!.WithAnalyzers(ImmutableArray.Create<DiagnosticAnalyzer>(
    new FakerAnalyzer(),
    new ExporterAnalyzer()
)).GetAllDiagnosticsAsync();

/*
 * Dumps the output of the analyzers run. Without the filters, this will also output several messages regarding
 * global using statements and unnecessary usings that aren't even displayed by VS itself. These are just noise
 * for this usage. 
 */
foreach (var d in result.Where(d=>!(d.IsSuppressed || d.Severity == DiagnosticSeverity.Hidden)))
{
    Console.WriteLine(d.ToString());
}

//var driver = CSharpGeneratorDriver.Create(new AutoExportGenerator());
//driver.RunGenerators(compilation!);

//Console.WriteLine(driver.GetRunResult().Dump());