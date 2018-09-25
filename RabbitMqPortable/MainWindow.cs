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
        private string sysHomeDrive = null;
        private string sysHomeDirectory = null;
        private string erlangDirectory = null;
        private string ertsDirectory = null;
        private string rmqDirectory = null;
        private string dataDirectory = null;
        private Process process = null;

        /// <summary>
        /// Default constructor
        /// </summary>
        public MainWindow()
        {
            InitializeComponent();

            // Find a location of RabbitMqPortable.exe
            UriBuilder uri = new UriBuilder(Assembly.GetExecutingAssembly().CodeBase);
            homeDirectory = Path.GetDirectoryName(Uri.UnescapeDataString(uri.Path));
            sysHomeDrive = homeDirectory[1] == ':' ? homeDirectory.Substring(0, 2) : "C:";
            sysHomeDirectory = homeDirectory[1] == ':' ? homeDirectory.Substring(2) + @"\data\" : "\\";
        }

        /// <summary>
        /// On form load ... 
        /// Prepare all settings and variables for succesfull 
        /// server start (and stop) and check if everything is fine
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
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
                else if (rp.ToLower().Equals("data"))
                {
                    dataDirectory = d; // Save it.. 
                }
            }

            //********************************************************************
            // OK.. Now let's check if we find everything we need .... 
            if (String.IsNullOrEmpty(erlangDirectory))
            { 
                MessageBox.Show("Cant find erlang directory", this.Text, MessageBoxButtons.OK, MessageBoxIcon.Stop);
                Close();
                return;
            }
            else if (String.IsNullOrEmpty(ertsDirectory))
            { 
                MessageBox.Show("Cant find erts directory inside erlang directory. Look like you have invalid or uncomplete erlang directory", 
                                this.Text, 
                                MessageBoxButtons.OK, 
                                MessageBoxIcon.Stop);
                Close();
                return;
            }
            else if(String.IsNullOrEmpty(rmqDirectory))
            {
                MessageBox.Show("Cant find rabbitmq directory.", 
                                this.Text, 
                                MessageBoxButtons.OK, 
                                MessageBoxIcon.Stop);
                Close();
                return;
            }


            if (String.IsNullOrEmpty(dataDirectory))
            {
                dataDirectory = Path.Combine(homeDirectory, "data");
                try
                {
                    Directory.CreateDirectory(dataDirectory);
                }
                catch
                {
                    MessageBox.Show("Cant create '"+dataDirectory+"' directory.",
                                    this.Text,
                                    MessageBoxButtons.OK,
                                    MessageBoxIcon.Stop);
                    Close();
                    return;
                }
            }
            //*********************************************************************************
            //  OK.. Now update erl.ini with actual paths
            string iniFile = Path.Combine(erlangDirectory, "bin\\erl.ini");
            try
            {
                string iniContent = "[erlang]\nBindir={0}\nProgname=erl\nRootdir={1}\n";

                string nc = String.Format(iniContent, ertsDirectory.Replace(@"\", @"\\"),
                                                      erlangDirectory.Replace(@"\", @"\\"));

                File.WriteAllText(iniFile, nc);
            }
            catch (Exception ex)
            {
                MessageBox.Show("Error updating " + iniFile + "\n\n" + ex.Message,
                                this.Text,
                                MessageBoxButtons.OK,
                                MessageBoxIcon.Stop);
                Close();
                return;
            }

            //***************************************************************************
            // Prepare bat file for 'start console' ... 
            string bat_start = "@set ERLANG_HOME=" + erlangDirectory + "\\\n";
            bat_start += "@set RABBITMQ_BASE=" + homeDirectory + "\\data\\\n";
            bat_start += "@set HOMEDRIVE=" + sysHomeDrive + "\n";
            bat_start += "@set HOMEPATH=" + sysHomeDirectory + "\n";
            bat_start += "@cmd.exe";
            File.WriteAllText(Path.Combine(rmqDirectory, @"sbin\startShell.bat"), bat_start);
            
            WriteLineToOutput("", Color.White); // Add one empty line....

            if (!StartServer())     // And finnaly try to start server ...
            {
                Close();
                return;
            }
        }

        /// <summary>
        /// Capture main process stderr output
        /// and display it on main window
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        private void process_ErrorDataReceived(object sender, DataReceivedEventArgs e)
        {
            try
            {
                if (e.Data != null)
                {
                    System.Diagnostics.Debug.WriteLine(e.Data);
                    this.Invoke(new MethodInvoker(delegate
                    {
                        WriteLineToOutput(e.Data, Color.Red);
                    }));
                }
            }
            catch (Exception ex)
            { 
            }
        }
        
        /// <summary>
        /// Capture main process stdout output
        /// and display it on main window
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        private void process_OutputDataReceived(object sender, DataReceivedEventArgs e)
        {
            try
            {
                if (e.Data != null)
                {
                    System.Diagnostics.Debug.WriteLine(e.Data);
                    this.Invoke(new MethodInvoker(delegate
                    {
                        WriteLineToOutput(e.Data, Color.LimeGreen);
                    }));
                }
            }
            catch (Exception ex)
            { 
            }
        }

        /// <summary>
        /// On form closing...
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        private void Form1_FormClosing(object sender, FormClosingEventArgs e)
        {
            StopServer();
        }

        /// <summary>
        /// On 'Start console' menu item
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        private void startConsoleToolStripMenuItem_Click(object sender, EventArgs e)
        {
            Process console = new Process();
            console.StartInfo.UseShellExecute = false;
            console.StartInfo.FileName = Path.Combine(rmqDirectory, @"sbin\startShell.bat");
            console.StartInfo.WorkingDirectory = Path.Combine(this.rmqDirectory, "sbin");
            console.Start();
        }

        /// <summary>
        /// On 'Start server' menu item
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        private void startServerToolStripMenuItem_Click(object sender, EventArgs e)
        {
            StartServer();
        }

        /// <summary>
        /// On 'Stop console' menu item
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        private void stopServerToolStripMenuItem_Click(object sender, EventArgs e)
        {
            StopServer();
        }

        /// <summary>
        /// Start RabbitMQ server in portable mode
        /// </summary>
        /// <returns>True if succesfull</returns>
        private bool StartServer()
        {
            //*********************************************************************************
            //  OK.. Now prepare process with envirovment vars that run RabbitMQ server 
            //  and kick it
            try
            {
                process = new Process();
                process.StartInfo.EnvironmentVariables["ERLANG_HOME"] = erlangDirectory + @"\"; // Where is erlang ? 
                process.StartInfo.EnvironmentVariables["RABBITMQ_BASE"] = homeDirectory + @"\data\";  // Where to put RabbitMQ logs and database
                process.StartInfo.EnvironmentVariables["HOMEDRIVE"] = sysHomeDrive;     // Erlang need this for cookie file
                process.StartInfo.EnvironmentVariables["HOMEPATH"] = sysHomeDirectory;  // Erlang need this for cookie file
                process.StartInfo.UseShellExecute = false;
                process.StartInfo.CreateNoWindow = true;
                process.StartInfo.RedirectStandardOutput = true;
                process.StartInfo.RedirectStandardError = true;
                process.OutputDataReceived += new DataReceivedEventHandler(process_OutputDataReceived);
                process.ErrorDataReceived += new DataReceivedEventHandler(process_ErrorDataReceived);
                process.StartInfo.FileName = "cmd.exe";
                process.StartInfo.Arguments = "/c \"" + Path.Combine(rmqDirectory, @"sbin\rabbitmq-server.bat") + "\"";

                WriteLineToOutput(" Server started ... ", Color.White);

                process.Start();
                process.BeginOutputReadLine();

                startServerToolStripMenuItem.Enabled = false;
                stopServerToolStripMenuItem.Enabled = true;
                
                return true;
            }
            catch (Exception ex)
            {
                MessageBox.Show("Error starting server\n\n" + ex.Message,
                                this.Text,
                                MessageBoxButtons.OK,
                                MessageBoxIcon.Stop);
                return false;
            }

        }

        /// <summary>
        /// Stop RabbitMQ server
        /// </summary>
        /// <returns>True if succesfull</returns>
        private bool StopServer()
        {
            try
            {
                if (process != null && !process.HasExited)
                {
                    process.Kill(); // Kill main process

                    // OK.. Now check if Erlang leave something behind .. ?!
                    Process[] allProcesses = Process.GetProcesses();
                    foreach (Process p in allProcesses)
                    {
                        try
                        {
                            string fullPath = p.MainModule.FileName;
                            if (fullPath != null && fullPath.StartsWith(erlangDirectory))
                            {
                                System.Diagnostics.Debug.WriteLine("Force to kill : " + fullPath);
                                p.Kill();
                            }
                        }
                        catch (Exception ex)
                        {
                        }
                    }
                }
                process = null;

                startServerToolStripMenuItem.Enabled = true;
                stopServerToolStripMenuItem.Enabled = false;

                WriteLineToOutput(" Server stoped ... ", Color.White);

                return true;
            }
            catch (Exception ex)
            {
                MessageBox.Show("Error stoping server\n\n" + ex.Message,
                                this.Text,
                                MessageBoxButtons.OK,
                                MessageBoxIcon.Stop);
                return false;
            }
        }

        /// <summary>
        /// Write line in specified color to output window
        /// </summary>
        /// <param name="data">String to be written</param>
        /// <param name="c">Color to be used</param>
        private void WriteLineToOutput(string data, Color c)
        {
            richTextBox1.SelectionColor = c;
            if (data != null)
                richTextBox1.AppendText(data + "\n");
            richTextBox1.ScrollToCaret();
        }
    }
}