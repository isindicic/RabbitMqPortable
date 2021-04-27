using System;
using System.Diagnostics;
using System.Drawing;
using System.IO;
using System.Reflection;
using System.Windows.Forms;

namespace RabbitMqPortable
{
    public partial class MainWindow : Form
    {
        private string dataDirectory;
        private string erlangDirectory;
        private string ertsDirectory;
        private readonly string homeDirectory;
        private Process process;
        private string rmqDirectory;
        private readonly string sysHomeDirectory;
        private readonly string sysHomeDrive;

        /// <summary>
        ///     Default constructor
        /// </summary>
        public MainWindow()
        {
            InitializeComponent();

            // Find a location of RabbitMqPortable.exe
            var uri = new UriBuilder(Assembly.GetExecutingAssembly().CodeBase);
            homeDirectory = Path.GetDirectoryName(Uri.UnescapeDataString(uri.Path));
            sysHomeDrive = homeDirectory[1] == ':' ? homeDirectory.Substring(0, 2) : "C:";
            sysHomeDirectory = homeDirectory[1] == ':' ? homeDirectory.Substring(2) + @"\data\" : "\\";
        }

        /// <summary>
        ///     On form load ...
        ///     Prepare all settings and variables for succesfull
        ///     server start (and stop) and check if everything is fine
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        private void Form1_Load(object sender, EventArgs e)
        {
            //********************************************************************
            // OK.. Now let's check which directories we have there ... 
            var dirs = Directory.GetDirectories(homeDirectory);
            foreach (var d in dirs)
            {
                var rp = d.Replace(homeDirectory, "").Replace("\\", ""); // Get just a directory name
                if (rp.ToLower().StartsWith("erl")) // if this is erlang directory.. 
                {
                    erlangDirectory = d; // OK.. This save this as base erlang directory.. 
                    var edirs = Directory.GetDirectories(erlangDirectory); // Now search inside.. 
                    foreach (var ed in edirs)
                    {
                        var erp = ed.Replace(erlangDirectory, "").Replace("\\", "");
                        if (erp.StartsWith("erts")) // And if this is erts ...
                            ertsDirectory = Path.Combine(erlangDirectory, erp + "\\bin"); // Save it.. 
                    }

                    tsErlang.Text = rp;
                }
                else if (rp.ToLower().StartsWith("rabbit")) // if this is rabbit-mq directory.. 
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
            if (string.IsNullOrEmpty(erlangDirectory))
            {
                MessageBox.Show("Cant find erlang directory", Text, MessageBoxButtons.OK, MessageBoxIcon.Stop);
                Close();
                return;
            }

            if (string.IsNullOrEmpty(ertsDirectory))
            {
                MessageBox.Show(
                    "Cant find erts directory inside erlang directory. Look like you have invalid or uncomplete erlang directory",
                    Text,
                    MessageBoxButtons.OK,
                    MessageBoxIcon.Stop);
                Close();
                return;
            }

            if (string.IsNullOrEmpty(rmqDirectory))
            {
                MessageBox.Show("Cant find rabbitmq directory.",
                    Text,
                    MessageBoxButtons.OK,
                    MessageBoxIcon.Stop);
                Close();
                return;
            }


            if (string.IsNullOrEmpty(dataDirectory))
            {
                dataDirectory = Path.Combine(homeDirectory, "data");
                try
                {
                    Directory.CreateDirectory(dataDirectory);
                }
                catch
                {
                    MessageBox.Show("Cant create '" + dataDirectory + "' directory.",
                        Text,
                        MessageBoxButtons.OK,
                        MessageBoxIcon.Stop);
                    Close();
                    return;
                }
            }

            //*********************************************************************************
            //  OK.. Now update erl.ini with actual paths
            var iniFile = Path.Combine(erlangDirectory, "bin\\erl.ini");
            try
            {
                var iniContent = "[erlang]\nBindir={0}\nProgname=erl\nRootdir={1}\n";

                var nc = string.Format(iniContent, ertsDirectory.Replace(@"\", @"\\"),
                    erlangDirectory.Replace(@"\", @"\\"));

                File.WriteAllText(iniFile, nc);
            }
            catch (Exception ex)
            {
                MessageBox.Show("Error updating " + iniFile + "\n\n" + ex.Message,
                    Text,
                    MessageBoxButtons.OK,
                    MessageBoxIcon.Stop);
                Close();
                return;
            }

            //***************************************************************************
            // Prepare bat file for 'start console' ... 
            var bat_start = "@setlocal\n";
            bat_start += "@set ERLANG_HOME=" + erlangDirectory + "\\\n";
            bat_start += "@set RABBITMQ_BASE=" + homeDirectory + "\\data\\\n";
            bat_start += "@set RABBITMQ_CONFIG_FILE=" + homeDirectory + "\\data\\rabbitmq\n";
            bat_start += "@set RABBITMQ_LOG_BASE=" + homeDirectory + "\\data\\log\n";
            bat_start += "@set LOGS=\n";

            bat_start += "@set HOMEDRIVE=" + sysHomeDrive + "\n";
            bat_start += "@set HOMEPATH=" + sysHomeDirectory + "\n";
            bat_start += "@cmd.exe";
            File.WriteAllText(Path.Combine(rmqDirectory, @"sbin\startShell.bat"), bat_start);

            WriteLineToOutput("", Color.White); // Add one empty line....

            if (!StartServer()) // And finnaly try to start server ...
                Close();
        }

        /// <summary>
        ///     Capture main process stderr output
        ///     and display it on main window
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        private void process_ErrorDataReceived(object sender, DataReceivedEventArgs e)
        {
            try
            {
                if (e.Data != null)
                {
                    Debug.WriteLine(e.Data);
                    Invoke(new MethodInvoker(delegate { WriteLineToOutput(e.Data, Color.Red); }));
                }
            }
            catch (Exception)
            {
            }
        }

        /// <summary>
        ///     Capture main process stdout output
        ///     and display it on main window
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        private void process_OutputDataReceived(object sender, DataReceivedEventArgs e)
        {
            try
            {
                if (e.Data != null)
                {
                    Debug.WriteLine(e.Data);
                    Invoke(new MethodInvoker(delegate { WriteLineToOutput(e.Data, Color.LimeGreen); }));
                }
            }
            catch (Exception)
            {
            }
        }

        /// <summary>
        ///     On form closing...
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        private void Form1_FormClosing(object sender, FormClosingEventArgs e)
        {
            StopServer();
        }

        /// <summary>
        ///     On 'Start console' menu item
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        private void startConsoleToolStripMenuItem_Click(object sender, EventArgs e)
        {
            var console = new Process
            {
                StartInfo =
                {
                    UseShellExecute = false,
                    FileName = Path.Combine(rmqDirectory, @"sbin\startShell.bat"),
                    WorkingDirectory = Path.Combine(rmqDirectory, "sbin")
                }
            };
            console.Start();
        }

        /// <summary>
        ///     On 'Start server' menu item
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        private void startServerToolStripMenuItem_Click(object sender, EventArgs e)
        {
            StartServer();
        }

        /// <summary>
        ///     On 'Stop console' menu item
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        private void stopServerToolStripMenuItem_Click(object sender, EventArgs e)
        {
            StopServer();
        }

        /// <summary>
        ///     Start RabbitMQ server in portable mode
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
                process.StartInfo.EnvironmentVariables["RABBITMQ_BASE"] =
                    homeDirectory + @"\data\"; // Where to put RabbitMQ logs and database
                process.StartInfo.EnvironmentVariables["RABBITMQ_CONFIG_FILE"] =
                    homeDirectory + @"\data\rabbitmq"; // Where is config file
                process.StartInfo.EnvironmentVariables["RABBITMQ_LOG_BASE"] =
                    homeDirectory + @"\data\log"; // Where are log files
                process.StartInfo.EnvironmentVariables.Remove("LOGS");

                process.StartInfo.EnvironmentVariables["HOMEDRIVE"] = sysHomeDrive; // Erlang need this for cookie file
                process.StartInfo.EnvironmentVariables["HOMEPATH"] =
                    sysHomeDirectory; // Erlang need this for cookie file
                process.StartInfo.UseShellExecute = false;
                process.StartInfo.CreateNoWindow = true;
                process.StartInfo.RedirectStandardOutput = true;
                process.StartInfo.RedirectStandardError = true;
                process.OutputDataReceived += process_OutputDataReceived;
                process.ErrorDataReceived += process_ErrorDataReceived;
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
                    Text,
                    MessageBoxButtons.OK,
                    MessageBoxIcon.Stop);
                return false;
            }
        }

        /// <summary>
        ///     Stop RabbitMQ server
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
                    var allProcesses = Process.GetProcesses();
                    foreach (var p in allProcesses)
                        try
                        {
                            var fullPath = p.MainModule.FileName;
                            if (fullPath != null && fullPath.StartsWith(erlangDirectory))
                            {
                                Debug.WriteLine("Force to kill : " + fullPath);
                                p.Kill();
                            }
                        }
                        catch (Exception)
                        {
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
                    Text,
                    MessageBoxButtons.OK,
                    MessageBoxIcon.Stop);
                return false;
            }
        }

        /// <summary>
        ///     Write line in specified color to output window
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