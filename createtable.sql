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

create table adresse_staging (
                    adresse_id serial primary key,
                    foreign key (sujet_id) references gou.table_sujet(sujet_id),
                    adresse_clb smallint,
                    precision varchar(20),
                    source_codage varchar(20),
                    geometry geometry(POINT, 2154)
                    );
                    
