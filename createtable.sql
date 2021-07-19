/* 
* creation des tables 
* et remplissage de ces dernières
* pour dbgouramic
*/  

create table gou.t_sujet (
                    sujet_id char(7) primary key,
                    date_naissance date not null
                    );


\cd /home/defuneste/Documents/recherche/gouramic/gouramic-db-analyses/data
\! pwd

\copy gou.t_sujet  from 'sujet.csv' delimiter ',' ;

create table gou.t_adresse (
                    geometry geometry(POINT, 2154),
                    adresse_id serial primary key,
                    sujet_id char(7) references gou.t_sujet(sujet_id),
                    adresse_clb varchar(20),
                    precision smallint,
                    source_codage varchar(20)
                    );

-- ici j'ai changé le delimiter pour mieux gérer le WKT                    
-- \copy gou.adresse_staging from 'adresse.csv' delimiter ';' ;

-- une version avec un shpe pe plus simple -I mets en place un gist sur geometry
-- vu que je vais faire un import derrière je sais pas si c'est le plus pertinent

-- cette commande est une version "localhost" penser à changer port, user, adresse ..
-- on est en bash et plus psql 
-- chenager aussi le chemin d'adresse
shp2pgsql -s 2154 -g geometry -I data/adresse.shp staging.adresse_staging | psql -h localhost -U postgres -p 5432 -d dbgouramic

-- on repasse en psql
insert into gou.t_adresse (adresse_id, sujet_id, adresse_clb, precision, source_codage, geometry)
select adrss_d, sujet_d, adrss_c, precisn, src_cdg, geometry
from staging.adresse_staging;


-- la table des interval de temps
create table gou.t_interval_date(
                    interval_id smallint primary key,
                    date_start date,
                    date_end date
                    );
                    
 \cd /home/defuneste/Documents/recherche/gouramic/gouramic-db-analyses/data
\! pwd
 
\copy gou.t_interval_date  from 'table_interval_date.csv' delimiter ',' ;
                   
-- la table de passage, la pk est la interval_id + adresse_id
create table gou.p_t_adresse_interval (
                    adresse_id smallint references gou.t_adresse(adresse_id),
                    interval_id smallint references gou.t_interval_date(interval_id)
                    );

\copy gou.p_t_adresse_interval  from 'p_table_adresse_interval.csv' delimiter ',' ;