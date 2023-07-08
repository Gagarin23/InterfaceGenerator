using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Text;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Text;

namespace InterfaceGenerator;

[AttributeUsage(AttributeTargets.Class)]
public class GenerateInterfaceAttribute : Attribute
{

}

[Generator]
public class Generator : ISourceGenerator
{
    public void Initialize(GeneratorInitializationContext context)
    {
        // No initialization required for this generator
    }

    public void Execute(GeneratorExecutionContext context)
    {
        var compilation = context.Compilation;
        var proxyAttributeSymbol = compilation.GetTypeByMetadataName(typeof(GenerateInterfaceAttribute).FullName);

        foreach (var tree in compilation.SyntaxTrees)
        {
            var model = compilation.GetSemanticModel(tree);
            var classes = tree.GetRoot().DescendantNodesAndSelf().OfType<ClassDeclarationSyntax>();

            foreach (var @class in classes)
            {
                if (model.GetDeclaredSymbol(@class) is INamedTypeSymbol classSymbol && classSymbol.GetAttributes().Any(attr => attr.AttributeClass.Equals(proxyAttributeSymbol, SymbolEqualityComparer.Default)))
                {
                    var interfaceName = "I" + classSymbol.Name;
                    var stringBuilder = new StringBuilder();

                    var namespaceName = classSymbol.ContainingNamespace.ToDisplayString();

                    stringBuilder.AppendLine($"namespace {namespaceName};");

                    stringBuilder.AppendLine("public interface " + interfaceName);
                    stringBuilder.AppendLine("{");

                    var methods = GetMethods(classSymbol);
                    var properties = classSymbol.GetMembers().OfType<IPropertySymbol>().Where(m => m.DeclaredAccessibility is Accessibility.Public or Accessibility.Protected).ToList();
                    var events = classSymbol.GetMembers().OfType<IEventSymbol>().Where(m => m.DeclaredAccessibility is Accessibility.Public or Accessibility.Protected).ToList();

                    GenerateMethods(methods, stringBuilder);
                    GenerateProperties(properties, stringBuilder);
                    GenerateEvents(events, stringBuilder);

                    stringBuilder.AppendLine("}");
                    stringBuilder.AppendLine();

                    stringBuilder.AppendLine($"public partial class {classSymbol.Name} : {interfaceName}");
                    stringBuilder.AppendLine("{ }");

                    context.AddSource(classSymbol.Name + ".g.cs", SourceText.From(stringBuilder.ToString(), Encoding.UTF8));
                }
            }
        }
    }

    private static void GenerateMethods(IEnumerable<IMethodSymbol> methods, StringBuilder stringBuilder)
    {
        foreach (var method in methods)
        {
            if (method.DeclaredAccessibility is Accessibility.Public
                && method is { IsStatic: false, IsOverride: false }
                && method.Name != ".ctor" && !method.Name.StartsWith("get_") && !method.Name.StartsWith("set_") && !method.Name.StartsWith("add_") && !method.Name.StartsWith("remove_"))
            {
                var genericParams = method.IsGenericMethod ? $"<{string.Join(", ", method.TypeParameters.Select(tp => tp.Name))}>" : string.Empty;

                var returnType = method.ReturnType.ToDisplayString();
                var parameters = string.Join(", ", method.Parameters.Select(p => $"{p.Type.ToDisplayString()} {p.Name}"));
                var constraints = GenerateGenericConstraints(method.TypeParameters);

                stringBuilder.AppendLine($"    {returnType} {method.Name}{genericParams}({parameters}){constraints};");
            }
        }
    }

    private static void GenerateProperties(IEnumerable<IPropertySymbol> properties, StringBuilder stringBuilder)
    {
        foreach (var property in properties)
        {
            var returnType = property.Type.ToDisplayString();
            stringBuilder.AppendLine($"    {returnType} {property.Name} {{ get; set; }}");
        }
    }

    private static void GenerateEvents(IEnumerable<IEventSymbol> events, StringBuilder stringBuilder)
    {
        foreach (var eventSymbol in events)
        {
            var eventType = eventSymbol.Type.ToDisplayString();
            stringBuilder.AppendLine($"    event {eventType} {eventSymbol.Name};");
        }
    }

    private static IEnumerable<IMethodSymbol> GetMethods(INamedTypeSymbol classSymbol)
    {
        if (classSymbol == null)
        {
            return Array.Empty<IMethodSymbol>();
        }

        var methods = new List<IMethodSymbol>();
        methods.AddRange(classSymbol.GetMembers().OfType<IMethodSymbol>());

        if (classSymbol.BaseType != null && classSymbol.BaseType.Name != "Object")
        {
            methods.AddRange(GetMethods(classSymbol.BaseType));
        }

        return methods;
    }

    private static string GenerateGenericConstraints(ImmutableArray<ITypeParameterSymbol> typeParameters)
    {
        var constraints = new StringBuilder();

        foreach (var typeParam in typeParameters)
        {
            if (typeParam.ConstraintTypes.Length > 0 || typeParam.HasReferenceTypeConstraint || typeParam.HasValueTypeConstraint || typeParam.HasNotNullConstraint || typeParam.HasConstructorConstraint)
            {
                constraints.AppendLine();
                constraints.Append($"    where {typeParam.Name} : ");

                var constraintList = new List<string>();

                if (typeParam.HasReferenceTypeConstraint)
                {
                    constraintList.Add("class");
                }

                if (typeParam.HasUnmanagedTypeConstraint)
                {
                    constraintList.Add("unmanaged");
                }
                else if (typeParam.HasValueTypeConstraint)
                {
                    constraintList.Add("struct");
                }

                if (typeParam.HasNotNullConstraint)
                {
                    constraintList.Add("notnull");
                }

                if (typeParam.HasConstructorConstraint)
                {
                    constraintList.Add("new()");
                }

                constraints.Append(string.Join(", ", constraintList));
            }
        }

        return constraints.ToString();
    }
}
