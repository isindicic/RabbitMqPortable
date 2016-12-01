# RabbitMQ portable

Launch [RabbitMQ](https://www.rabbitmq.com/) server for Windows without need to install it from USB.

To successfully compile solution you'll need Visual Studio 2005 or compatible. Solution targets .NET 2.0 and application require such framework version installed in order to run.

##How to quickly start to use portable RabbitMQ ?

* Download [latest release](https://github.com/isindicic/RabbitMqPortable/releases/download/1.0/RabbitMqPortable.zip) and unpack it somewhere. Start RabbitMqPortable.exe and wait for server to start up. To terminate server simply close window.

##How to prepare portable RabbitMQ from scratch ?

* Copy RabbitMQPortable.exe to some directory on USB stick or hard drive (let's call it RMQP directory)
* Download RabbitMQ zip from this [page](https://www.rabbitmq.com/install-windows-manual.html) and unpack it in RMQP directory.
* Get Erlang directory and copy it to RMQP directory. Unfortunately there are no zip distribution, so you should install it somewhere, get files and unistall it or take it from your friend computer :) . At the end you should see something like this:
```
E:\RabbitMQPortable>dir
 Volume in drive E is SINDA_HD
 Volume Serial Number is 747B-8319

 Directory of E:\RabbitMQPortable

30.11.2016.  08:56    <DIR>          .
30.11.2016.  08:56    <DIR>          ..
28.11.2016.  07:41    <DIR>          erl7.2.1
28.11.2016.  07:41    <DIR>          rabbitmq_server-3.6.6
30.11.2016.  07:52           761.856 RabbitMqPortable.exe
               1 File(s)        761.856 bytes
               4 Dir(s)  276.252.303.360 bytes free

E:\RabbitMQPortable>
```

* Run RabbitMQPortable.exe. If everything is fine it should locate erlang and rabbit and run it


##How to manage and/or configure portable RabbitMQ server ?

Usually those job are done using rabbitmqctl.bat or rabbitmq-plugins.bat from sbin directory but if you start it directly it will nag that can't find (portable) Erlang. 
So to get this task done use 'Start console' command from application main menu and use previously mentioned bat files from console window that will be opened.

##Where to put rabbitmq.config file ? 

Application keep all data into 'data' directory. If this directory don't exist it will be automaticaly created when you start application first time. Put rabbitmq.config to data directory and server should find it.
