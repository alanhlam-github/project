-- World population database query

SELECT sum(population)
FROM world

SELECT distinct(continent)
FROM world

-- Africa
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

SELECT continent, count(name)
FROM world
GROUP BY continent

SELECT continent,count(name)
FROM world
WHERE population >= 10000000
GROUP BY continent

SELECT continent
FROM world
WHERE population >= 100000000
GROUP BY continent