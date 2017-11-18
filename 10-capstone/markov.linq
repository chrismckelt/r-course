<Query Kind="Program">
  <Output>DataGrids</Output>
  <NuGetReference>MarkovSharp</NuGetReference>
  <Namespace>log4net</Namespace>
  <Namespace>log4net.Appender</Namespace>
  <Namespace>log4net.Config</Namespace>
  <Namespace>log4net.Core</Namespace>
  <Namespace>log4net.DateFormatter</Namespace>
  <Namespace>log4net.Filter</Namespace>
  <Namespace>log4net.Layout</Namespace>
  <Namespace>log4net.Layout.Pattern</Namespace>
  <Namespace>log4net.ObjectRenderer</Namespace>
  <Namespace>log4net.Plugin</Namespace>
  <Namespace>log4net.Repository</Namespace>
  <Namespace>log4net.Repository.Hierarchy</Namespace>
  <Namespace>log4net.Util</Namespace>
  <Namespace>log4net.Util.TypeConverters</Namespace>
  <Namespace>MarkovSharp</Namespace>
  <Namespace>MarkovSharp.Models</Namespace>
  <Namespace>MarkovSharp.TokenisationStrategies</Namespace>
  <Namespace>Newtonsoft.Json</Namespace>
  <Namespace>Newtonsoft.Json.Bson</Namespace>
  <Namespace>Newtonsoft.Json.Converters</Namespace>
  <Namespace>Newtonsoft.Json.Linq</Namespace>
  <Namespace>Newtonsoft.Json.Schema</Namespace>
  <Namespace>Newtonsoft.Json.Serialization</Namespace>
  <Namespace>Sanford.Collections</Namespace>
  <Namespace>Sanford.Collections.Generic</Namespace>
  <Namespace>Sanford.Collections.Immutable</Namespace>
  <Namespace>Sanford.Multimedia</Namespace>
  <Namespace>Sanford.Multimedia.Midi</Namespace>
  <Namespace>Sanford.Multimedia.Midi.UI</Namespace>
  <Namespace>Sanford.Multimedia.Timers</Namespace>
  <Namespace>Sanford.Threading</Namespace>
  <Namespace>System.Runtime.InteropServices</Namespace>
</Query>

//https://github.com/chriscore/MarkovSharp
void Main()
{
	const string saved = @"c:\temp\markov.model";
	bool load_from_disk = false;

	var model = new StringMarkov(1);
	if (load_from_disk == false)
	{
		string lines = GetLines();
		model.Learn(lines);
		model.Save(saved);
	}
	else
	{
		model.Load<StringMarkov>(saved, 1);
	}

	for (int i = 2; i < 6; i++)
	{
		if (Search(model, i))
		{
			Console.WriteLine("SUCCESS")			;
		}
	}
}
	public static string text = CleanString.RemoveSpecialCharacters("When you breathe, I want to be the air for you. I'll be there for you, I'd live and I'd");

private bool Search(StringMarkov model, int level)
{
	try {
		model.Retrain(level);
		string text = CleanString.RemoveSpecialCharacters("When you breathe, I want to be the air for you. I'll be there for you, I'd live and I'd");
		
		var splits = text.Split(Convert.ToChar(" "));
		
		var sb = new StringBuilder();
		int tmp = 0;
		foreach (var split in splits)
		{
			if (tmp > splits.Count() - level)
			{	
				sb.Append(splits[tmp]);
			}
			tmp++;
		}
		
		sb.ToString().Dump();
		
		model.GetMatches(sb.ToString())
		 .Where(a =>
							a != model.GetPrepadGram()
							&& a != model.GetTerminatorGram()
						)
						.GroupBy(a => a)
						.OrderByDescending(a => a.Count())
						.Select(a => new { Value = a.Key, Total = a.Count()}).ToArray()
						.Dump();
		return true;
	}
	catch (Exception ex){
		ex.Dump();
		return false;
	}
	
}

private string GetLines()
{
	const string dir = @"C:\dev\r-course\10-capstone\data\final\en_US";
	var sb = new StringBuilder();

	foreach (var file in Directory.GetFiles(dir))
	{
		Console.WriteLine($"Starting {file}");
		var fileStream = new FileStream(file, FileMode.Open, FileAccess.Read);
		using (var streamReader = new StreamReader(fileStream, Encoding.UTF8))
		{
			int max = Int32.MaxValue / 2;
			string line;
			while ((line = streamReader.ReadLine()) != null && max > 1)
			{
				sb.AppendLine(CleanString.RemoveSpecialCharacters(line));
				max--;
				//Console.WriteLine(max);
			}
		}
	}

	string lines = sb.ToString();
	return lines;
}



public class CleanString
{
	public static string RemoveSpecialCharacters(string str)
	{
		string data = Path.GetInvalidFileNameChars().Aggregate(str, (current, c) => current.Replace(c.ToString(), string.Empty)).ToLower();
		return data
		.Replace(Convert.ToChar("'"), ' ')
		.Replace(Convert.ToChar("."), ' ')
		.Replace(Convert.ToChar(","), ' ');
	}

}