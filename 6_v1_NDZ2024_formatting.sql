--- Tagad Pārbaude anonimizācijai

--1) Tabulā v1_NDZ2024 izveido aili rs_kod
ALTER TABLE v1_NDZ2024
ADD rs_kod varchar(11);

--2) Tabulas v1_NDZ2024 ailē [ps_kod] nošķir.
SELECT *
FROM v1_NDZ2024
WHERE LEFT(ps_kod, 1) != 'G' AND LEFT(ps_kod, 1) != '003' -- Te nav neviena LEFT(ps_kod, 1) = 'K'
ORDER BY ps_kod; 

--3) No tabulas ailes [ps_kod] pārcel uz aili [rs_kod] tās vērtības.
UPDATE v1_NDZ2024
SET rs_kod = ps_kod
WHERE LEFT(ps_kod, 1) != 'G' AND LEFT(ps_kod, 1) != '003';

----Pārbaude
SELECT *
FROM [dbo].[v1_NDZ2024]
WHERE LEFT(ps_kod, 1) != 'G' AND LEFT(ps_kod, 1) != '003';

--4) No ailes [ps_kod] izdzēs vērtības, kas Pārcletas uz aili [rs_kod]
UPDATE [dbo].[v1_NDZ2024]
SET ps_kod = NULL
WHERE LEFT(ps_kod, 1) != 'G' AND LEFT(ps_kod, 1) != '003';

----Pārbaude
SELECT *
FROM [dbo].[v1_NDZ2024]
WHERE LEFT(ps_kod, 1) != 'G' AND LEFT(ps_kod, 1) != '003';

--5) Veicam šifrēšanu aile rs_kod
--5.1. Uz brīdi tabulas v1_NDZ2024 aili [ps_kod] Pārsaucam par [ps_kod1], jo tabulā shiData arī ir aile ar nosaukumu [ps_kod]
	EXEC sp_rename 'v1_NDZ2024.ps_kod', 'ps_kod1', 'COLUMN';

--5.2. Tagad savieno ar shiData tabulu uz ailes [rs_kod]
	SELECT v1_NDZ2024.*, shiData.*
	FROM v1_NDZ2024
	JOIN[zdd].dbo.shiData ON v1_NDZ2024.rs_kod = shiData.rs_kod;
-- Pārbaude
	SELECT *
    FROM [dbo].[v1_NDZ2024]
    WHERE LEFT(ps_kod1, 1) != 'G' AND LEFT(ps_kod1, 1) != '003';
--nevienam rs_kod neatradām

--5.3. Atmaini tabulas v1_NDZ2024 aili [ps_kod1] atpakaļ uz [ps_kod].
	EXEC sp_rename 'v1_NDZ2024.ps_kod1', 'ps_kod', 'COLUMN';

--6) Tabulā v1_NDZ2024 aili [rs_kod] pārsauc par d_kodi, jo tā tas ir Tabulas I555
	EXEC sp_rename 'v1_NDZ2024.rs_kod', 'd_kodi', 'COLUMN';
