--drop table if exists source_schema;
--drop table if exists data_virtualization;
--drop table if exists view_definition;

CREATE TABLE IF NOT EXISTS data_virtualization 
  ( 
     id          VARCHAR(255) NOT NULL, 
     description VARCHAR(4096), 
     NAME        VARCHAR(1024) UNIQUE, 
     PRIMARY KEY (id) 
  ); 

CREATE TABLE IF NOT EXISTS source_schema 
  ( 
     id   VARCHAR(255) NOT NULL, 
     ddl  VARCHAR(1000000), 
     PRIMARY KEY (id) 
  ); 

CREATE TABLE IF NOT EXISTS view_definition 
  ( 
     id           VARCHAR(255) NOT NULL, 
     complete     BOOLEAN NOT NULL, 
     ddl          VARCHAR(100000), 
     description  VARCHAR(4096), 
     NAME         VARCHAR(1024) UNIQUE, 
     state        VARCHAR(100000), 
     user_defined BOOLEAN NOT NULL, 
     view_name    VARCHAR(1024), 
     PRIMARY KEY (id) 
  ); 
