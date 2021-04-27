using System;
using System.Windows.Forms;

namespace RabbitMqPortable
{
    internal static class Program
    {
        /// <summary>WWW
        /// The main entry point for the application.
        /// </summary>
        [STAThread]
        private static void Main()
        {
            Application.EnableVisualStyles();
            Application.SetCompatibleTextRenderingDefault(false);
            Application.Run(new MainWindow());
        }
    }
}