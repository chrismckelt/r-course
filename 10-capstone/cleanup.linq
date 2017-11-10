<Query Kind="Program" />

/*
	Helper script to remove data files
*/
void Main()
{
	File.Delete(@"C:\dev\r-course\10-capstone\sqldb_pcorpus_mydata");

	foreach (var f in Directory.GetFiles(@"C:\dev\r-course\10-capstone\data"))
	{
		File.Delete(f);
	}
}

 