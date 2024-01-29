USE [Zinas ND]
GO

-- izmanto preNSZ_2024
-- datne nāk ar daudzām tukšām rindām; MS SQL procesā to izkoriģēt nevar
-- 19_ndz dati nāk pēdiņās visam; pēcāk formatējot noņemt

BULK INSERT preNDZ_2024
FROM '...\dati\N_2024\NDZ2024_CSVformat_forMSSQL\19_NDZ_2024.csv'
WITH (
	FIELDTERMINATOR = '|',
	ROWTERMINATOR = '\n',
	FIRSTROW = 2,
	CODEPAGE = 'ACP'
);
