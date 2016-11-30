# RabbitMQ portable

Launch [RabbitMQ](https://www.rabbitmq.com/) server without need to install it and/or from USB.

To successfully compile solution you'll need Visual Studio 2005 or compatible. Solution targets .NET 2.0 and application require such framework version installed in order to run.

##How to prepare portable RabbitMQ

* Copy RabbitMQPortable.exe to some directory on USB stick or hard drive (let's call it RMQP directory)
* Download [RabbitMQ zip](https://www.rabbitmq.com/install-windows-manual.html) and unpack it in RMQP directory.
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




