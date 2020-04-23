Description of data wrangling challenge:
	- Big-picture questions:
		- In response to covid-19, what factors impacted governors' decisions to issue stay-at-home orders?
		- How have stay-at-home orders affected subsequent covid-related cases/deaths?
	- Data provided:
		- Time series data with confirmed cases/deaths from Johns Hopkins' Center for Systems Science and Engineering
		- Data indicating which states have issued stay-at-home orders, and when those orders were issued (as of 04-23-20)
	- Challenge:
		- Find a way to merge the information contained in these datasets
		- For each state, plot confirmed cases/deaths over time, and provide a visual marker for when states issued a stay-at-home order
	- Bonus:
		- Is there a pattern underlying when different states issued stay-at-home orders? This is an open-ended question (I certainly don't know). If you have any particular hypotheses, it will likely require that you find other sources of data to test them.

Data sources:
	- Download the most recent time series data for covid19 confirmed cases and deaths from
		https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_time_series
	- Stay-at-home dates obtained on 04-23-20 from
		https://www.finra.org/rules-guidance/key-topics/covid-19/shelter-in-place

Pro-tip:
When reading data from a file on Github, use the URL corresponding to the RAW data, not the pretty data. When you click on a file that lives inside a Github repository, it'll take you to the "pretty" view. For example:
https://github.com/CSSEGISandData/COVID-19/blob/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv

You won't be able to read that file. You'll need to click on the "Raw" button to access the raw data. How do you know it's raw data? You'll know it when you see it. For example:
https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv