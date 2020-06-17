/* 
* creation des tables 
* et remplissage de ces dernières
* pour dbgouramic
*/  

create table gou.table_sujet (
                    sujet_id char(7) primary key,
                    date_naissance date not null
                    );


\cd /home/defuneste/Documents/recherche/gouramic/gouramic-db-analyses/data
\! pwd

\copy gou.table_sujet  from 'sujet.csv' delimiter ',' ;

create table gou.table_adresse (
                    adresse_id serial primary key,
                    sujet_id char(7) references gou.table_sujet(sujet_id),
                    adresse_clb smallint,
                    precision varchar(20),
                    source_codage varchar(20),
                    geometry geometry(POINT, 2154)
                    );
                    
-- ici j'ai changé le delimiter pour mieux gérer le WKT                    
\copy gou.adresse_staging from 'adresse.csv' delimiter ';' ;

-- une version avec un shpe pe plus simple -I mets en place un gist sur geometry
-- vu que je vais faire un import derrière je sais pas si c'est le plus pertinent

-- je suis passer par une version "non localhost"" de cette commande
shp2pgsql -s 2154 -g geometry -I data/adresse.shp staging.adresse_staging | psql -h localhost -U postgres -p 5432 -d dbgouramic

insert into gou.table_adresse (adresse_id, sujet_id, adresse_clb, precision, source_codage, geometry)
select adrss_d, sujet_d, adrss_c, rslt_ty, sorc_lc, geometry
from staging.adresse_staging;

