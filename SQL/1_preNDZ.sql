USE [NDZ_DB]
GO

/* 1. Sākumā aiļu platumu neskatos, to izformēs pēc tam.
      NDZ dati nāk ar daudzām tukšām rindām, tās jāiztīra.
      Sāku ar tabulu preNDZ_yyyy tabulu, kuru pēcāk izdzēsīšu.*/

CREATE TABLE preNDZ_2022 (
	NM_code varchar(300),
	DN_code varchar(300),
	sandat varchar(300),
	zinkod varchar(300),
	[period] varchar(300));

/* 2. No SS eksportēto CSV failu (t19_ndz_yyyy_noSS.csv) ielādē SQL tabulā preNDZ_YYYY.
      Dati nāk pēdiņās visam; pēcāk formatējot tās noņem.*/

BULK INSERT preNDZ_2022
FROM '..\NDZ_codes\data\noSS\t19_ndz_2022_noSS.csv'
WITH (
	FIELDTERMINATOR = '|',
	ROWTERMINATOR = '\n',
	FIRSTROW = 2,
	CODEPAGE = 'ACP'
);  
-- 2022: 1 277 706 rindas

/* 3. Tabulā preNDZ_yyyy ir daudz tukšo rindu
      Vispirms izņem tās.*/

DELETE FROM [dbo].[preNDZ_2022]
WHERE NM_code IS NULL AND DN_code IS NULL AND sandat = '""' AND zinkod IS NULL AND [period] IS NULL;  
-- 2022: bija 1 277 706 rindas, izdzēsu 0 rindas. Palika 1 277 706 rindas.

/* 4. Izformē atribūtu NM_code.*/
-- Noņem pēdiņas
   UPDATE preNDZ_2022 SET NM_code = REPLACE(NM_code, '"', ''); 

-- Nomaini ailes platumu.
   ALTER TABLE preNDZ_2022 ALTER COLUMN NM_code VARCHAR(11);

-- Pārbaudu uz NULL
  SELECT * FROM preNDZ_2022 WHERE NM_code IS NULL;  -- 2022: Nuļļu nav

/* 5. Izformē atribūtu DN_code.*/
-- Noņem pēdiņas
   UPDATE preNDZ_2022 SET DN_code = REPLACE(DN_code, '"', '');

-- Nomaini ailes platumu.
   ALTER TABLE preNDZ_2022 ALTER COLUMN DN_code VARCHAR(11);

-- Pārbaudu uz NULL
   SELECT * FROM preNDZ_2022 WHERE DN_code IS NULL;  -- 2022: Nuļļu nav

/* 6. Izformē aili sandat. */
-- Noņem pēdiņas
   UPDATE preNDZ_2022 SET sandat = REPLACE(sandat, '"', '');

-- Pārbaudu uz NULL
   SELECT * FROM preNDZ_2022 WHERE sandat IS NULL;  -- 2022: Nuļļu nav

-- Nomaina ailes tipu uz datumu
   ALTER TABLE preNDZ_2022 ADD NewsanDat DATE;
   UPDATE preNDZ_2022 SET NewsanDat = CONVERT(DATE, sandat, 104);

   ALTER TABLE preNDZ_2022 DROP COLUMN sandat;
   EXEC sp_rename 'preNDZ_2022.NewsanDat', 'sandat', 'COLUMN';

/* 7. Izformē aili zinkod. */
-- Noņem pēdiņas
   UPDATE preNDZ_2022 SET zinkod = REPLACE(zinkod, '"', '');

-- Nomaini ailes platumu.
   ALTER TABLE preNDZ_2022 ALTER COLUMN zinkod VARCHAR(2);

-- Pārbaudu uz NULL
   SELECT * FROM preNDZ_2022 WHERE zinkod IS NULL;  -- 2022: Nuļļu nav 

/* 9. Izformē aili period. */
-- Noņem pēdiņas
   UPDATE preNDZ_2022 SET [period] = REPLACE(period, '"', '');

-- Nomaini ailes platumu.
   ALTER TABLE preNDZ_2022 ALTER COLUMN [period] VARCHAR(6);

-- Pārbaudu uz NULL
   SELECT * FROM preNDZ_2022 WHERE [period] IS NULL;  -- 2022: Nuļļu nav

/*10. No SS nākušajos datos ir rindas, kur mēnesis starp ziņojuma saņemšanas mēnesi un norādīto periodu atšķiras.
     Citreiz pat par vairākiem mēnešiem starpā. 2022 gadā tādu ir 1554 rindu.

    LĒMUMS: Tiek pieņemts lēmums tos pārdefinēt atbilstoši periodam, ko nes NDZ saņemšanas datums.*/

    SELECT zinkod, sandat, [period], DATEPART(MONTH, sandat) AS sandat_month, SUBSTRING([period], 5, 2) AS period_month
    FROM preNDZ_2022
    WHERE DATEPART(MONTH, sandat) != SUBSTRING([period], 5, 2);

-- Par cik visām citām vērtībām rēķinām datumus mēnesī no sandat, tāpēc te atšķirības ailē [period] tiks piemērotas ailes [sandat] mēnesim.
-- Apskati:
    SELECT sandat, [period] = FORMAT(DATEPART(YEAR, sandat), '0000') + FORMAT(DATEPART(MONTH, sandat), '00') FROM preNDZ_2022
    WHERE DATEPART(MONTH, sandat) != SUBSTRING([period], 5, 2) ORDER BY sandat;

-- Izmaini:
    UPDATE preNDZ_2022 SET [period] = FORMAT(DATEPART(YEAR, sandat), '0000') + FORMAT(DATEPART(MONTH, sandat), '00') FROM preNDZ_2022
    WHERE DATEPART(MONTH, sandat) != SUBSTRING([period], 5,2); --2022. gadam izmainītas: 1554 rindas 

/* 11. Tagad noliec atsevišķā tabulā tās vērtības, kurām sandat ir no iepriekšējā gada. */
-- Apskati:
   SELECT * FROM preNDZ_2022 WHERE LEFT([period], 4) = CAST(RIGHT('preNDZ_2022', 4) AS INT) - 1 ORDER BY sandat;

-- Punkti 11.1, 11.2, 11.3 attiecās tikai uz 2021. gadu
-- 11.1 Izveido pagaidu tabulu ar 2020. gada vērtībām no tabulas preNDZ_2021.
   CREATE TABLE p2020_noNDZ2021 (NM_code VARCHAR(11), DN_code VARCHAR(11), zinkod VARCHAR(2), [period] VARCHAR(6), sandat DATE);

-- 11.2 Ievieto rindas no iepriekšējā gada jaunizveidotajā tabulā
   INSERT INTO p2020_noNDZ2021 (NM_code, DN_code, zinkod, [period], sandat)
   SELECT NM_code, DN_code, zinkod, [period], sandat FROM preNDZ_2021
   WHERE LEFT([period], 4) = CAST(RIGHT('preNDZ_2021', 4) AS INT) - 1 ORDER BY sandat;

-- 11.3 Izdzēs rindas, kuras tika pārvietotas uz tabulu p2020_noNDZ2021
   DELETE FROM preNDZ_2021 WHERE LEFT([period], 4) = CAST(RIGHT('preNDZ_2021', 4) AS INT) - 1;

-- 11.4 Pārbaudi
   SELECT * FROM preNDZ_2022 WHERE LEFT([period], 4) = CAST(RIGHT('preNDZ_2022', 4) AS INT) - 1;

/* Tabulā preNDZ_2021 bija 1 320 405 rindas. 
   Uz tabulu p2020_noNDZ2022 tika pārnestas 344. 
   Palika 1 320 061 rindas. */

/* Tabulā preNDZ_2022 bija 1 277 706 rindas.
   Un visas palika, jo nebija jāpārnes rindas.*/

/* 12. Tabulā [dbo].[preNDZ_2022] pārsauc aili [DN_code] uz [PS_code], jo citur aile [DN_code] jau tiek lietota.*/
   EXEC sp_rename 'preNDZ_2022.DN_code', 'PS_code', 'COLUMN';

-- Pārbauda, vai ir vēl kādi lieki PS_code
   SELECT * FROM [dbo].[preNDZ_2022]
   WHERE LEFT(PS_code, 1) != 'P' AND LEFT(PS_code, 1) != '9' AND LEFT(PS_code, 2) != '32' AND LEFT(PS_code, 2) != '38' AND PS_code != '00000000003' AND LEFT(PS_code, 1) != 'T'
   ORDER BY PS_code; 
