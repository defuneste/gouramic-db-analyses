/* 
* creation DB gouramic + setup des roles  
* devra sans doite etre refait sur une version 
* plus récente de Postgre et postgis
* cela sera important pour les rasters
*/  

-- penser à le decommenter ....
-- create database dbgouramic;

-- pour éviter un fourre tout dans public postgis à son propre schema
create schema if not exists postgis; 
create extension postgis schema postgis;
-- A partir de la version 3.0 de postgis uniquement
create extension postgis_raster schema postgis;
grant usage on schema postgis to public;
-- ici pe un pas possible sur ancienne version
create schema if not exists gou;
-- un schema pour les données en transit
create schema if not exists staging;

alter database dbgouramic set search_path=public, postgis, contrib;
alter database dbgouramic set postgis.gdal_enabled_drivers = 'ENABLE_ALL';


-- penser à relancer le service
-- sudo service postgresql restart
-- verification de postgis 

SELECT postgis_full_version();

-- creation des roles et users

create role lecteur NOCREATEDB NOCREATEROLE;
grant CONNECT on database dbgouramic 
                       to lecteur;
grant USAGE 
         on schema gou 
         to lecteur;
-- ici il y a pas de table pour le moment mais je garde pour memoire
grant select on all TABLES 
                        in schema gou 
                        to lecteur;
-- changer les param par defaut 
alter default PRIVILEGES 
                      in SCHEMA gou 
                      grant select on TABLES 
                      to lecteur;
-- les fonctions sont dans public donc normalement accessible à verif

create user matthieu with PASSWORD 'unpwd'; 
grant lecteur TO matthieu;
create user remi with PASSWORD 'deuxpwd';
grant lecteur to remi;

-- penser à relancer le service
-- sudo service postgresql restart
