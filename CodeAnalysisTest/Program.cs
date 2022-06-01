using System.Linq.Expressions;
using Bogus;
using Bogus.DataSets;

namespace CodeAnalysisTest;

internal class Program
{
    static void Main(string[] args)
    {
        Console.WriteLine("Hello, World!");
    }
}


public class TestEntity
{
    public int Id { get; set; }
    public string Name { get; set; } = "";

    public string IgnoredByExport { get; set; } = "";
    public DateTimeOffset Created { get; set; } = DateTimeOffset.Now;
    public ICollection<ChildEntity> Children { get; set; } = new List<ChildEntity>();
}

public class ChildEntity
{
    public int Id { get; set; }
    public string? Name { get; set; }

    public int ParentId { get; set; }
    public TestEntity Parent { get; set; } = null!;

    public int RelatedId { get; set; }
    public RelatedEntity Related { get; set; } = null!;
}

public class RelatedEntity
{
    public int Id { get; set; }
    public string Name { get; set; } = "";
}

public sealed class TestEntityFaker : Faker<TestEntity>
{
    public TestEntityFaker(int seed = 42, bool useIds = false)
    {
        UseSeed(seed);
        RuleFor(e => e.Id, (f) => useIds ? f.IndexFaker : 0);
        RuleFor(e => e.Name, f => f.Commerce.ProductName());
        RuleFor(e => e.Children, (f, e) => (new ChildEntityFaker(e, f.Random.Int(), useIds)).Generate(f.Random.Int(0, 5)));
        RuleFor(e => e.IgnoredByExport, "");
        RuleFor(e => e.Created, f => f.Date.PastOffset());
    }
}

public sealed class ChildEntityFaker : Faker<ChildEntity>
{
    public ChildEntityFaker(TestEntity parent, int seed = 1337, bool useIds = false)
    {
        UseSeed(seed);

        RuleFor(e => e.Id, (f) => useIds ? f.IndexFaker : 0);
        RuleFor(e => e.Name, f => f.Commerce.ProductName());
        RuleFor(e => e.ParentId, parent.Id);
        RuleFor(e => e.Parent, parent);

        RuleFor(e => e.Related, (f) => (new RelatedEntityFaker(f.Random.Int(), useIds)).Generate());
        RuleFor(e => e.RelatedId, (f, e) => e.Related.Id);
    }
}

public sealed class RelatedEntityFaker : Faker<RelatedEntity>
{
    public RelatedEntityFaker(int seed = 0xbad1dea, bool useIds = false)
    {
        UseSeed(seed);
        RuleFor(e => e.Id, (f) => useIds ? f.IndexFaker : 0);
        RuleFor(e => e.Name, f => f.Commerce.ProductName());
    }
}

public abstract class Exporter<TEntity>
{
    public void Ignore<TProperty>(Expression<Func<TEntity, TProperty>> property)
    {

    }
}

public abstract class AutoExporter<TEntity> : Exporter<TEntity>
{

}

public class TestEntityExport : Exporter<TestEntity>
{
    private int Id { get; }
    public string Created { get; }
    public string Name { get; }
    public List<ChildEntityExporter> Children { get; }

    public TestEntityExport(TestEntity entity)
    {
        Id = entity.Id;
        this.Id = entity.Id;
        Name = entity.Name;
        Created = entity.Created.ToString("G");
        Created = $"{entity.Created:G}";
        Children = entity.Children.Select(c => new ChildEntityExporter(c)).ToList();

        Ignore(m => m.IgnoredByExport);
    }
}

public class ChildEntityExporter : Exporter<ChildEntity>
{
    public ChildEntityExporter(ChildEntity entity)
    {

    }
}

//public class TestEntityAutoExporter : AutoExporter<TestEntity>
//{

//}