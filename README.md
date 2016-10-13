
emq_auth_mysql
==============

Authentication, ACL with MySQL Database

Notice: changed mysql driver to [mysql-otp](https://github.com/mysql-otp/mysql-otp).

Build Plugin
-------------

make && make tests

Configure Plugin
----------------

File: etc/emq_auth_mysql.conf

```
## Mysql Server
auth.mysql.server = 127.0.0.1:3306

## Mysql Pool Size
auth.mysql.pool = 8

## Mysql Username
## auth.mysql.username =

## Mysql Password
## auth.mysql.password =

## Mysql Database
auth.mysql.database = mqtt

## Variables: %u = username, %c = clientid

## Authentication Query: select password only
auth.mysql.auth_query = select password from mqtt_user where username = '%u' limit 1

## Password hash: plain, md5, sha, sha256, pbkdf2
auth.mysql.passwd_hash = sha256

## %% Superuser Query
auth.mysql.super_query = select is_superuser from mqtt_user where username = '%u' limit 1

## ACL Query Command
auth.mysql.acl_query = select allow, ipaddr, username, clientid, access, topic from mqtt_acl where ipaddr = '%a' or username = '%u' or username = '$all' or clientid = '%c'

## ACL nomatch
auth.mysql.acl_nomatch = deny
```

Import mqtt.sql
---------------

Import mqtt.sql into your database.

Load Plugin
-----------

./bin/emqttd_ctl plugins load emq_auth_mysql

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

License
-------

Apache License Version 2.0

Author
-------

Feng Lee <feng@emqtt.io>.

