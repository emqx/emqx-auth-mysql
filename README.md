
emqttd_plugin_mysql
===================

emqttd Authentication, ACL with MySQL Database

Notice: changed mysql driver to [mysql-otp](https://github.com/mysql-otp/mysql-otp).


Build Plugin
------------

This project is a plugin for emqttd broker. In emqttd project:

If the submodule exists:

```
git submodule update --remote
```

Orelse:

```
git submodule add https://github.com/emqtt/emqttd_plugin_mysql.git plugins/emqttd_plugin_mysql

make && make dist
```

Configure Plugin
----------------

File: etc/plugin.config

```erlang
[

{emqttd_plugin_mysql, [

    {mysql_pool, [
        %% ecpool options
        {pool_size, 4},
        {auto_reconnect, 3},

        %% mysql options
        {host,     "localhost"},
        {port,     3306},
        {user,     ""},
        {password, ""},
        {database, "mqtt"},
        {encoding, utf8}
    ]},

    %% Variables: %u = username, %c = clientid, %a = ipaddress

    %% Superuser Query
    {superquery, "select is_superuser from mqtt_user where username = '%u' limit 1"},

    %% Authentication Query: select password only
    {authquery, "select password from mqtt_user where username = '%u' limit 1"},

    %% hash algorithm: md5, sha, sha256, pbkdf2?
    {password_hash, sha256},

    %% select password with salt
    %% {authquery, "select password, salt from mqtt_user where username = '%u'"},

    %% sha256 with salt prefix
    %% {password_hash, {salt, sha256}},

    %% sha256 with salt suffix
    %% {password_hash, {sha256, salt}},

    %% comment this query, the acl will be disabled
    {aclquery, "select * from mqtt_acl where ipaddr = '%a' or username = '%u' or username = '$all' or clientid = '%c'"},

    %% If no rules matched, return...
    {acl_nomatch, allow}
]}
].
```

Import mqtt.sql
---------------

Import mqtt.sql to your database.

Load Plugin
-----------

```
./bin/emqttd_ctl plugins load emqttd_plugin_mysql
```

Auth Table
----------

Notice: This is a demo table. You could authenticate with any user table.

```sql
CREATE TABLE `mqtt_user` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `username` varchar(100) DEFAULT NULL,
  `password` varchar(100) DEFAULT NULL,
  `salt` varchar(20) DEFAULT NULL,
  `is_superuser` tinyint(1) DEFAULT 0,
  `created` datetime DEFAULT NULL,
  PRIMARY KEY (`id`),
  UNIQUE KEY `mqtt_username` (`username`)
) ENGINE=MyISAM DEFAULT CHARSET=utf8;
```

ACL Table
----------

```sql
CREATE TABLE `mqtt_acl` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `allow` int(1) DEFAULT NULL COMMENT '0: deny, 1: allow',
  `ipaddr` varchar(60) DEFAULT NULL COMMENT 'IpAddress',
  `username` varchar(100) DEFAULT NULL COMMENT 'Username',
  `clientid` varchar(100) DEFAULT NULL COMMENT 'ClientId',
  `access` int(2) NOT NULL COMMENT '1: subscribe, 2: publish, 3: pubsub',
  `topic` varchar(100) NOT NULL DEFAULT '' COMMENT 'Topic Filter',
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
```

Support
-------

Fork this project and implement your own authentication/ACL mechanism.

Contact feng@emqtt.io if any issues.

License
-------

Apache License Version 2.0

