--1 Izveido tabulu NDZyyyy
CREATE TABLE NDZ2022 (
    [period] varchar(6),
    NDZ_sanemsanas_datums date,
    PS_code varchar(11),
	DN_code varchar(11),
	NM_code varchar(11),
	zinkod varchar(2));

/* LĒMUMS 2 
2.1 Tāpēc dzēšu šīs rindas un sekojošās tiek ievietotas tabulā NDZ2022. */
INSERT INTO NDZ2022 ([period], NDZ_sanemsanas_datums, PS_code, NM_code, zinkod)
SELECT DISTINCT [period], sandat, PS_code, NM_code, zinkod
FROM preNDZ_2022
WHERE PS_code != '00000000003'; 

/*2021. gads: 1 308 416 rindas (bija 1 320 061 preNDZ_yyyy, no tām noņēma 3522 ar PS_code '00000000003' 
              un 8 123 bija dubultās rindas.)
  2022. gads: 1 268 521 rindas (bija 1 277 706 preNDZ_yyyy, no tām noņēma 6412 ar PS_code '00000000003' 
              un 2 773 bija dubultās rindas.*/

--2.2 Pārbaude
SELECT * FROM NDZ2022 ORDER BY [period]; --Atceries, ka te vēl viss gads ir kopā.

--3 Tabulas nosaukuma maiņa no NDZyyyy uz NDZ2022_1NF
EXEC sp_rename 'NDZ2022', 'NDZ2022_1NF';

/*4 Nošķir PS_code no DN_code. 
    (Šajā solī var meklēt iespēju tos ps.) */

--4.1 Apskati
SELECT * FROM NDZ2022_1NF
WHERE LEFT(PS_code, 1) != 'P' AND LEFT(PS_code, 1) != '9' AND LEFT(PS_code, 1) != 'T' AND LEFT(PS_code, 2) != '32' AND LEFT(PS_code, 2) != '38'
ORDER BY PS_code;  --2021. gadā: Ailē [PS_code] ir 14 noPS rindas
                      --2022. gadā: Ailē [PS_code] ir 19 noPS rindas

--4.2 No tabulas ailes [PS_code] pārcel uz aili [DN_code] tās vērtības, kuras nav PS.
UPDATE NDZ2022_1NF SET DN_code = PS_code
WHERE LEFT(PS_code, 1) != 'P' AND LEFT(PS_code, 1) != '9' AND LEFT(PS_code, 1) != 'T' AND LEFT(PS_code, 2) != '32' AND LEFT(PS_code, 2) != '38';

--4.3 No tabulas ailes [PS_code] dzēs ierakstus, kuri tika pārcelti uz aili [DN_code]
UPDATE NDZ2022_1NF SET PS_code = NULL
WHERE LEFT(PS_code, 1) != 'P' AND LEFT(PS_code, 1) != '9' AND LEFT(PS_code, 1) != 'T' AND LEFT(PS_code, 2) != '32' AND LEFT(PS_code, 2) != '38';

--5 Tabulā NDZyyyy_1NF pievieno aili [sak_beidz]
ALTER TABLE NDZ2022_1NF ADD sak_beidz VARCHAR(1);

/*6 Tabulas NDZyyyy_1NF aile [sak_beidz] nes kodu '1' tiem kortežiem, kuru [zinkod] atribūts  '11', '14', '16', '41', '51', '54', '61', '92'.*/
SELECT NDZ_sanemsanas_datums, zinkod FROM NDZ2022_1NF
WHERE zinkod IN ('11', '14', '16', '41', '51', '54', '61', '92')
ORDER BY zinkod; --2021. gadā tādas ir 593 786 rindas
                 --2022. gadā tādas ir 604 562 rindas

UPDATE NDZ2022_1NF SET sak_beidz = '1'
WHERE zinkod IN ('11', '14', '16', '41', '51', '54', '61', '92');

SELECT zinkod, sak_beidz
FROM NDZ2022_1NF
ORDER BY zinkod; --2021 rindas: 1 308 416
                 --2022 rindas: 1 268 521

/*7 Tabulas NDZyyyy_1NF aile [sak_beidz] nes kodu '2' tiem kortežiem, kuru [zinkod] atribūts 21, 22, 23, 24, 25, 26, 40, 50, 53, 56, 72, 82, 91.*/
SELECT NDZ_sanemsanas_datums, zinkod, sak_beidz FROM NDZ2022_1NF
WHERE zinkod IN ('21', '22', '23', '24', '25', '26', '29', '40', '50', '53', '91') ORDER BY zinkod; 
--2021 rindas: 589 758
--2022 rindas: 524 816

UPDATE NDZ2022_1NF SET [sak_beidz] =  '2'
WHERE zinkod IN ('21', '22', '23', '24', '25', '26', '29', '40', '50', '53', '91');

SELECT NDZ_sanemsanas_datums, zinkod, sak_beidz
FROM NDZ2022_1NF ORDER BY zinkod; --2021 rindas: 1 308 416
                                  --2022 rindas: 1 268 521

--8 No tabulas NDZyyyy_1NF izdzēš tos kortežus, kuriem nav ziņojuma koda.
DELETE FROM NDZ2022_1NF
WHERE zinkod = '?';

/* 9 No tabulas NDZyyyy_1NF izdzēš tos kortežus, kuru ziņojuma kodi attiecas uz statusa maiņu:
     t.i., '13', '55',  '56',  '71', '72', '81', '82' un 'PM'. */
DELETE FROM NDZ2022_1NF
WHERE zinkod IN ('13', '55', '56', '71', '72', '81', '82', 'PM');

--2021. gada datnē ir 1 307 253 rindas. (Bija 1 308 416 rindas; tika izdzēstas 1163.) 
--2022. gada datnē ir 1 129 378 rindas. (Bija 1 268 521 rindas; tika izdzēstas 139 143.)
