/* 
* creation DB gouramic + setup des roles  
* devra sans doite etre refait sur une version 
* plus récente de Postgre et postgis
* cela sera important pour les rasters
*/  

CREATE DATABASE gouramic_EVS;

-- pour éviter un fourre tout dans public postgis à son propre schema
CREATE SCHEMA postgis; 
GRANT USAGE ON schema postgis to public;
CREATE EXTENSION postgis SCHEMA postgis;
-- A partir de la version 3.0 de postgis uniquement
CREATE EXTENSION postgis_raster SCHEMA postgis;
-- ici pe un pas possible sur ancienne version
ALTER DATABASE gouramic_EVS SET search_path=public, postgis, contrib;
CREATE SCHEMA gouramic;


-- creation des roles et users

CREATE ROLE lecteur NOCREATEDB NOCREATEROLE;
GRANT CONNECT ON DATABASE gouramic_evs TO lecteur;
GRANT USAGE ON SCHEMA gouramic TO lecteur;
-- ici il y a pas de table pour le moment mais je garde pour memoire
GRANT SELECT ON ALL TABLES IN SCHEMA gouramic TO lecteur;
-- changer les param par defaut 
ALTER DEFAULT PRIVILEGES IN SCHEMA gouramix GRANT SELECT ON TABLES TO lecteur;
-- les fonctions sont dans public donc normalement accessible à verif

CREATE USER Mathieu WITH PASSWORD 'unpwd'; 
GRANT lecteur TO Mathieu;
CREATE USER Remi WITH PASSWORD 'deuxpwd';
GRANT lecteur TO Remi;

-- une table vide de test 


