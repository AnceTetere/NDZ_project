-- izmanto preNDZ_20XX

BULK INSERT preNDZ_2021
FROM '..\NDZ_codes\data\noSS_pirmdati\NDZ2021_pD.csv'
WITH (
	FIELDTERMINATOR = '|',
	ROWTERMINATOR = '\n',
	FIRSTROW = 2,
	CODEPAGE = 'ACP'
);  
