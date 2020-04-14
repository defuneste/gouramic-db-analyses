/* 
* creation des tables 
* et remplissage de ces dernières
* pour dbgouramic
*/  

create table table_sujet_stagging (
                    sujet char(7) primary key,
                    date_naissance date not null
                    );


\cd /home/defuneste/Documents/recherche/gouramic/gouramic-db-analyses/data
\! pwd

\copy table_sujet_stagging from 'sujet.csv' delimiter ',' ;