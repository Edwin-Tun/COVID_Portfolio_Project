/*
COVID-19 Data Exploration

Skills used: Joins, CTE's, Temp Tables, Derived Tables, Subquery, Windows Functions, Aggregate Functions, Creating Views, Converting Data Types

*/


-- Checking the data imported 
SELECT 
    *
FROM
    PortfolioProject.CovidDeaths
WHERE
    continent IS NOT NULL
        AND location LIKE '%ando%'
ORDER BY location , date; 




SELECT 
    location,
    date,
    total_cases,
    new_cases,
    total_deaths,
    population
FROM
    PortfolioProject.CovidDeaths
WHERE
    continent IS NOT NULL
ORDER BY location , date;



-- Total Case vs Total Deaths
-- Likelihood of dying if infected with COVID in your country over time
SELECT 
    location,
    date,
    total_cases,
    total_deaths,
    (total_deaths / total_cases) * 100 AS LikelihoodOfDying
FROM
    PortfolioProject.CovidDeaths
WHERE
    continent IS NOT NULL
        AND location LIKE '%kingdom%'
ORDER BY location , date;



-- Shows what percentage of population infected with Covid over time for each country
SELECT 
    location,
    date,
    population,
    total_cases,
    (total_cases / population) * 100 AS PercentPopulationInfected
FROM
    PortfolioProject.CovidDeaths
ORDER BY location , date;



-- Total Case vs Total Popultaion
-- Country with highest infection rate compared to population
SELECT 
    location,
    MAX(total_cases) AS total_cases,
    MAX(population) AS population,
    MAX((total_cases) / (population)) * 100 AS InfectedPercentage
FROM
    PortfolioProject.CovidDeaths
WHERE
    continent IS NOT NULL
GROUP BY location
ORDER BY InfectedPercentage DESC;



-- Country with the highest death number per population
SELECT 
    location,
    MAX(CAST(total_deaths AS UNSIGNED)) AS total_deaths
FROM
    PortfolioProject.CovidDeaths
WHERE
    continent IS NOT NULL
GROUP BY location
ORDER BY total_deaths DESC;



-- Total Deaths per Country vs Total Deaths Worldwide
WITH DeathsPerCountry AS (
SELECT 
    location,
    MAX(CAST(total_deaths AS UNSIGNED)) AS total_deaths,
    MAX(population) AS population
FROM
    PortfolioProject.CovidDeaths
WHERE
    continent IS NOT NULL
GROUP BY location
ORDER BY total_deaths DESC)

SELECT 
	location,
    total_deaths AS TotalDeathCountry,
    SUM(total_deaths) OVER(order by total_deaths desc) AS TotalDeathWorldwide 
FROM 
	DeathsPerCountry;



-- Country with the highest death rate
SELECT 
    location,
    MAX(CAST(total_deaths AS UNSIGNED)) AS total_deaths,
    MAX(population) AS population,
    ROUND(MAX((total_deaths) / (population)) * 100,3) AS DeathsPercentage
FROM
    PortfolioProject.CovidDeaths
WHERE
    continent IS NOT NULL
GROUP BY location
ORDER BY DeathsPercentage DESC;



-- Breaking thing down by continent
-- Continent with the highest death count
SELECT 
    location,
    MAX(CAST(total_deaths AS UNSIGNED)) AS TotalDeathsCount
FROM
    PortfolioProject.CovidDeaths
WHERE
    continent IS NULL
GROUP BY location
ORDER BY TotalDeathsCount DESC;



-- Continent's death count and infected rate using derived table
SELECT 
    continent, 
    SUM(CountryDeath) AS TotalDeaths,
    SUM(CountryInfected) AS TotalInfected
    -- (SUM(CountryDeath)/SUM(CountryInfected) *100) AS DeathPercentage
FROM
    (SELECT 
        continent,
		location,
		MAX(CAST(total_deaths AS UNSIGNED)) AS CountryDeath,
        MAX(total_cases) AS CountryInfected
    FROM
        PortfolioProject.CovidDeaths
    WHERE
        continent IS NOT NULL
    GROUP BY location) AS DeathPerCountryTable
GROUP BY continent
ORDER BY TotalDeaths Desc;



-- Death Count of Country vs Continent
SELECT 
    continent,
    location,
    CountryDeath AS DeathPerCountry, 
    SUM(CountryDeath) OVER(PARTITION BY continent /*ORDER BY continent,location*/) AS DeathPerContinent
FROM
    (SELECT 
        continent,
		location,
		MAX(CAST(total_deaths AS SIGNED)) AS CountryDeath
    FROM
        PortfolioProject.CovidDeaths
    WHERE
        continent IS NOT NULL
    GROUP BY location) AS DeathsPerCountryTable
ORDER BY DeathPerContinent DESC, DeathPerCountry DESC;



-- Using CTE for further calculation from the above query's results
-- The percentage of death per country of each continent
WITH CounvsCont AS 
(
SELECT 
    continent,
    location,
    CountryDeath AS DeathPerCountry, 
    SUM(CountryDeath) OVER(PARTITION BY continent /*ORDER BY continent,location*/) AS DeathPerContinent
FROM
    (
    SELECT 
        continent,
		location,
		MAX(CAST(total_deaths AS SIGNED)) AS CountryDeath
    FROM
        PortfolioProject.CovidDeaths
    WHERE
        continent IS NOT NULL
    GROUP BY location
    ) AS DeathsPerCountryTable
ORDER BY DeathPerContinent DESC, DeathPerCountry DESC
) 
SELECT 
    *,
    ROUND((DeathPerCountry / DeathPerContinent) * 100,
            3) AS DeathPercentage
FROM
    CounvsCont;
    
    
    
-- Global Number
SELECT 
--     date,
    SUM(new_cases) AS total_cases,
    SUM(new_deaths) AS total_deaths,
    ROUND(SUM(new_deaths) / SUM(new_cases) * 100,3) AS DeathPercentage
FROM
    PortfolioProject.CovidDeaths
WHERE
    continent IS NOT NULL;
-- Group By date




-- Total Population vs Vaccinations
-- Shows Percentage of Population that has recieved at least one Covid Vaccine
SELECT 
	dea.continent, 
	dea.location, 
	dea.date, 
	dea.population, 
	vac.new_vaccinations,
	SUM(CAST(vac.new_vaccinations AS SIGNED)) OVER (PARTITION BY dea.Location ORDER BY dea.location, dea.Date) AS RollingPeopleVaccinated
FROM 
	PortfolioProject.CovidDeaths dea
JOIN 
	PortfolioProject.CovidVaccinations vac
ON 
	dea.location = vac.location AND dea.date = vac.date
WHERE 
	dea.continent IS NOT NULL;



-- Using CTE to perform Calculation on Partition By in previous query
-- Show the percentage of population vaccinated per country
WITH PopvsVac AS
(
SELECT 
	dea.continent, 
	dea.location, 
	dea.date, 
	dea.population, 
	vac.new_vaccinations,
	SUM(CAST(vac.new_vaccinations AS SIGNED)) OVER (PARTITION BY dea.Location ORDER BY dea.location, dea.Date) AS RollingPeopleVaccinated
FROM 
	PortfolioProject.CovidDeaths dea
JOIN 
	PortfolioProject.CovidVaccinations vac
ON 
	dea.location = vac.location AND dea.date = vac.date
WHERE 
	dea.continent IS NOT NULL)

SELECT 
    *, (RollingPeopleVaccinated / Population) * 100 AS PercentageVaccinated
FROM
    PopvsVac;
    



-- Using Temp Table to store data and perform calculation on Partition By in previous query from Partition By in previous query
CREATE TEMPORARY TABLE PercentPopulationVaccinated(
	continent VARCHAR(255),
    location VARCHAR(255),
    date DATE,
    population INT,
    new_vaccinations INT,
    RollingPeopleVaccinated INT);
    
-- Insert data into the temporay table
INSERT INTO PercentPopulationVaccinated
SELECT 
	dea.continent, 
	dea.location, 
	dea.date, 
	dea.population, 
	vac.new_vaccinations,
	SUM(CAST(vac.new_vaccinations AS SIGNED)) OVER (PARTITION BY dea.Location ORDER BY dea.location, dea.Date) AS RollingPeopleVaccinated
FROM 
	PortfolioProject.CovidDeaths dea
JOIN 
	PortfolioProject.CovidVaccinations vac
ON 
	dea.location = vac.location AND dea.date = vac.date
WHERE 
	dea.continent IS NOT NULL;

-- Use the temporary table created to perform calculation on Partition By in previous query
SELECT 
    *, (RollingPeopleVaccinated / Population) * 100 AS PercentageVaccinated
FROM
    PercentPopulationVaccinated;
    



-- Creating View to store data
Create VIEW v_PercentPopulationVaccinated AS
SELECT 
	dea.continent, 
	dea.location, 
	dea.date, 
	dea.population, 
	vac.new_vaccinations,
	SUM(CAST(vac.new_vaccinations AS SIGNED)) OVER (PARTITION BY dea.Location ORDER BY dea.location, dea.Date) AS RollingPeopleVaccinated
FROM 
	PortfolioProject.CovidDeaths dea
JOIN 
	PortfolioProject.CovidVaccinations vac
ON 
	dea.location = vac.location AND dea.date = vac.date
WHERE 
	dea.continent IS NOT NULL;

-- Use the views created to perform calculation on Partition By in previous query
SELECT 
    *, (RollingPeopleVaccinated / Population) * 100 AS PercentageVaccinated
FROM
    v_PercentPopulationVaccinated;
    
