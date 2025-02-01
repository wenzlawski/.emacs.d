SELECT 
    o.name AS object_name,
    s.name AS schema_name,
    o.object_id,
    o.type_desc AS object_type,
    CASE
        WHEN o.type IN ('P', 'FN', 'IF', 'TF', 'TR') THEN CAST(sm.definition AS VARCHAR(MAX))  -- Functions, Procedures, Triggers
        WHEN o.type = 'U' THEN 'Table definition retrieved via INFORMATION_SCHEMA' -- Tables (we'll handle this case separately below)
        ELSE NULL
    END AS object_detail
FROM 
    sys.objects o
JOIN 
    sys.schemas s ON o.schema_id = s.schema_id
LEFT JOIN 
    sys.sql_modules sm ON o.object_id = sm.object_id  -- For retrieving source code (Functions, Procedures, Triggers)
WHERE 
    o.type IN ('P', 'FN', 'IF', 'TF', 'TR', 'U') -- Include procedures, functions, tables, triggers
    -- AND o.DT_MODIFIED > @Date

UNION ALL

SELECT 
    t.name AS object_name,
    s.name AS schema_name,
    t.object_id,
    'USER_TABLE' AS object_type,
    CAST(STRING_AGG(CAST(c.COLUMN_NAME + '\t' + c.DATA_TYPE AS VARCHAR(MAX)), '\n') AS VARCHAR(MAX)) AS object_detail  -- Using VARCHAR(MAX) for large aggregation
FROM 
    sys.tables t
JOIN 
    sys.schemas s ON t.schema_id = s.schema_id
JOIN 
    INFORMATION_SCHEMA.COLUMNS c ON t.name = c.TABLE_NAME AND s.name = c.TABLE_SCHEMA
GROUP BY 
    t.name, s.name, t.object_id
ORDER BY 
    schema_name, object_type, object_name;
