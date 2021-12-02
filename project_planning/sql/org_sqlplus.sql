SET AUTOTRACE TRACEONLY;
SET TIMING ON;
WHENEVER SQLERROR EXIT SQL.SQLCODE;
prompt QUERY_NEW_GeneTable;
SELECT o.*  FROM (SELECT f.* FROM (select
  listagg(ir.transcript_source_id, ', ') within group (order by ir.transcript_source_id) as transcript_ids
, ir.gene_source_id as source_id
, ir.project_id
, ir.interpro_db_name AS interpro_name
, ir.interpro_primary_id
, ir.interpro_secondary_id
, ir.interpro_desc
, ir.interpro_start_min
, ir.interpro_end_min
, ir.interpro_e_value
, ir.interpro_family_id
FROM
  ApidbTuning.interproresults ir
group by ir.project_id, ir.gene_source_id, ir.interpro_db_name, ir.interpro_primary_id, 
         ir.interpro_secondary_id, ir.interpro_desc, ir.interpro_start_min, 
         ir.interpro_end_min, ir.interpro_e_value, ir.gene_source_id, ir.interpro_family_id
order by
transcript_ids
, interpro_name
, interpro_start_min
) f WHERE f.source_id = 'PF3D7_0100100'
) o
;
exit
