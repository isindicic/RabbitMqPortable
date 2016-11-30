using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using System.Reflection;
using System.IO;
using System.Diagnostics;

namespace SindaSoft.RabbitMqPortable
{
    public partial class MainWindow : Form
    {
        private string homeDirectory = null;
        private string erlangDirectory = null;
        private string ertsDirectory = null;
        private string rmqDirectory = null;
        private Process process = null;

        public MainWindow()
        {
            InitializeComponent();

            // Find a location of RabbitMqPortable.exe
            UriBuilder uri = new UriBuilder(Assembly.GetExecutingAssembly().CodeBase);
            homeDirectory = Path.GetDirectoryName(Uri.UnescapeDataString(uri.Path));
        }

        private void Form1_Load(object sender, EventArgs e)
        {
            //********************************************************************
            // OK.. Now let's check which directories we have there ... 
            string[] dirs = Directory.GetDirectories(homeDirectory);
            foreach (string d in dirs)
            {
                string rp = d.Replace(homeDirectory, "").Replace("\\", ""); // Get just a directory name
                if (rp.ToLower().StartsWith("erl"))      // if this is erlang directory.. 
                {
                    erlangDirectory = d;    // OK.. This save this as base erlang directory.. 
                    string[] edirs = Directory.GetDirectories(erlangDirectory); // Now search inside.. 
                    foreach (string ed in edirs)
                    {
                        string erp = ed.Replace(erlangDirectory, "").Replace("\\", "");
                        if (erp.StartsWith("erts")) // And if this is erts ...
                        {
                            ertsDirectory = Path.Combine(erlangDirectory, erp + "\\bin");   // Save it.. 
                        }
                    }
                    tsErlang.Text = rp;
                }
                else if (rp.ToLower().StartsWith("rabbit"))     // if this is rabbit-mq directory.. 
                {
                    rmqDirectory = d; // save it.. 
                    tsRabbitMQ.Text = rp;
                }
            }

            //*********************************************************************************
            //  OK.. Now update erl.ini with actual paths
            string iniFile = Path.Combine(erlangDirectory, "bin\\erl.ini");
            string iniContent = "[erlang]\nBindir={0}\nProgname=erl\nRootdir={1}\n";

            string nc = String.Format(iniContent, ertsDirectory.Replace(@"\", @"\\"),
                                                  erlangDirectory.Replace(@"\", @"\\"));

            File.WriteAllText(iniFile, nc);

            //*********************************************************************************
            //  OK.. Now prepare process with envirovment vars that run RabbitMQ server 
            //  and kick it
            process = new Process();
            process.StartInfo.EnvironmentVariables["ERLANG_HOME"] = erlangDirectory + @"\"; // Where is erlang ? 
            process.StartInfo.EnvironmentVariables["RABBITMQ_BASE"] = homeDirectory + @"\data\";  // Where to put RabbitMQ logs and database
            process.StartInfo.UseShellExecute = false;
            process.StartInfo.CreateNoWindow = true;
            process.StartInfo.RedirectStandardOutput = true;
            process.StartInfo.RedirectStandardError = true;
            process.OutputDataReceived += new DataReceivedEventHandler(process_OutputDataReceived);
            process.ErrorDataReceived += new DataReceivedEventHandler(process_ErrorDataReceived);
            process.StartInfo.FileName = "cmd.exe";
            process.StartInfo.Arguments = "/c \"" + Path.Combine(rmqDirectory, @"sbin\rabbitmq-server.bat") + "\"";
            process.Start();
            process.BeginOutputReadLine();
        }

        private void process_ErrorDataReceived(object sender, DataReceivedEventArgs e)
        {
            System.Diagnostics.Debug.WriteLine(e.Data);
            try
            {
                this.Invoke(new MethodInvoker(delegate
                {
                    richTextBox1.SelectionColor = Color.Red;
                    if (e.Data != null)
                        richTextBox1.AppendText(e.Data + "\n");
                    richTextBox1.ScrollToCaret();
                }));
            }
            catch
            {
            }
        }
        
        private void process_OutputDataReceived(object sender, DataReceivedEventArgs e)
        {
            System.Diagnostics.Debug.WriteLine(e.Data);
            try
            {
                this.Invoke(new MethodInvoker(delegate
                {
                    richTextBox1.SelectionColor = Color.LimeGreen;
                    if (e.Data != null)
                        richTextBox1.AppendText(e.Data + "\n");
                    richTextBox1.ScrollToCaret();
                }));
            }
            catch
            { 
            }
        }

        private void Form1_FormClosing(object sender, FormClosingEventArgs e)
        {
            if (!process.HasExited)
            {
                //process.CloseMainWindow();
                process.Kill();
                foreach (Process p in Process.GetProcessesByName("erl"))
                    p.Kill();
            }
        }
    }
}