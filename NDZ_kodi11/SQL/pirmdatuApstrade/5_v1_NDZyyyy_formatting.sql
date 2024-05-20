--1) Tabulā v1_NDZ2021 izveido aili [pp_kods]
ALTER TABLE v1_NDZ2021
ADD pp_kods varchar(11);

--2) Tabulas v1_NDZ2021 ailē [PS_code] nošķir kodus.
SELECT *
FROM v1_NDZ2021
WHERE LEFT(PS_code, 1) != 'P' AND 
      LEFT(PS_code, 1) != '9' AND 
	  LEFT(PS_code, 1) != 'T' AND 
	  LEFT(PS_code, 2) != '32' AND 
	  LEFT(PS_code, 2) != '38'
ORDER BY PS_code; 

--3) No tabulas ailes [PS_code] pārcel uz aili [pp_kods] vērtības.
UPDATE v1_NDZ2021
SET pp_kods = PS_code
WHERE LEFT(PS_code, 1) != 'P' AND 
      LEFT(PS_code, 1) != '9' AND 
	  LEFT(PS_code, 1) != 'T' AND 
	  LEFT(PS_code, 2) != '32' AND 
	  LEFT(PS_code, 2) != '38';

--Pārbaude
SELECT *
FROM v1_NDZ2021
WHERE LEFT(PS_code, 1) != 'P' AND 
      LEFT(PS_code, 1) != '9' AND 
	  LEFT(PS_code, 1) != 'T' AND 
	  LEFT(PS_code, 2) != '32' AND 
	  LEFT(PS_code, 2) != '38';

--4) No ailes [PS_code] izdzēs vērtības, kas pārceltas uz aili [pp_kods]
UPDATE v1_NDZ2021
SET PS_code = NULL
WHERE LEFT(PS_code, 1) != 'P' AND 
      LEFT(PS_code, 1) != '9' AND 
	  LEFT(PS_code, 1) != 'T' AND 
	  LEFT(PS_code, 2) != '32' AND 
	  LEFT(PS_code, 2) != '38';

--Pārbaude
SELECT * FROM v1_NDZ2021
WHERE PS_code IS NULL;

/*5) Aile [pp_kods]*/
	SELECT A.*, B.*
	FROM v1_NDZ2021 AS A
	LEFT JOIN [ZDD].dbo.pa_DATI AS B 
	ON A.pp_kods = B.pp_kods
	WHERE A.PS_code IS NULL; 
	
--6) Tabulā v1_NDZ2021 aili [pp_kods] pārsauc par [DN_code].
EXEC sp_rename 'v1_NDZ2021.pp_kods', 'DN_code', 'COLUMN';
