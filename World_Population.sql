-- World population query

SELECT sum(population)
FROM world

SELECT distinct(continent)
FROM world

SELECT sum(gdp)
FROM world
WHERE continent = 'Africa'

SELECT count(name)
FROM world
WHERE area >=1000000

-- European countries
SELECT sum(population)
FROM world
WHERE name IN ('Estonia', 'Latvia', 'Lithuania')

SELECT continent,count(name)
FROM world
WHERE population >= 10000000
GROUP BY continent

SELECT name
FROM world
WHERE population > 
(SELECT population FROM world WHERE name='Russia')

-- GDP/population
SELECT name
FROM world
WHERE continent = 'europe' and gdp/population > 
(SELECT gdp/population FROM world WHERE name = 'united kingdom')

SELECT name,continent
FROM world
WHERE continent in (SELECT continent FROM world
WHERE name in ('Argentina','Australia'))
ORDER BY name