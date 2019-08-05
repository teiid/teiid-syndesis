--drop table if exists source_schema;
--drop table if exists data_virtualization;
--drop table if exists view_definition;

CREATE TABLE IF NOT EXISTS data_virtualization 
  ( 
     id          VARCHAR(64) NOT NULL, 
     description VARCHAR(4096), 
     name        VARCHAR(255) UNIQUE, 
     PRIMARY KEY (id) 
  ); 

CREATE TABLE IF NOT EXISTS source_schema 
  ( 
     id   VARCHAR(64) NOT NULL,
     name VARCHAR(255) UNIQUE, 
     ddl  VARCHAR(1000000), 
     PRIMARY KEY (id) 
  ); 

CREATE TABLE IF NOT EXISTS view_definition 
  ( 
     id           VARCHAR(64) NOT NULL, 
     complete     BOOLEAN NOT NULL, 
     ddl          VARCHAR(100000), 
     description  VARCHAR(4096), 
     name         VARCHAR(255), 
     state        VARCHAR(100000), 
     user_defined BOOLEAN NOT NULL, 
     dv_name      VARCHAR(255) NOT NULL, 
     PRIMARY KEY (id),
     UNIQUE (name, dv_name),
     FOREIGN KEY (dv_name) REFERENCES data_virtualization(name) ON DELETE CASCADE
  ); 

CREATE INDEX IF NOT EXISTS view_definition_dv_name ON view_definition(dv_name);
