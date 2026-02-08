
from selenium import webdriver
from selenium.common import NoSuchElementException
from selenium.webdriver.chrome.service import Service
from selenium.webdriver.chrome.options import Options
import pandas as pd
from selenium.webdriver.common.by import By
import time
from selenium.webdriver import ActionChains
from selenium.webdriver.common.keys import Keys
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC

path = "C:/Users/user/Documents/Productions Personnelles/Scraping/chromedriver-win64/chromedriver.exe"
chrome_binary_path = "C:/Users/user/Downloads/chrome-win64/chrome.exe"
service = Service(executable_path=path)

# Set up ChromeOptions
options = Options()

options.binary_location = chrome_binary_path  # Set the binary path to Chrome 140
driver = webdriver.Chrome(service=service, options=options)
web = "https://www.coolblue.be/fr/telephones-portables?redirect=smartphone"
driver.get(web)

cookiebutton=driver.find_element(By.NAME,"accept_cookie")
cookiebutton.click()



#Gather the list of all the smartphone you may find

def get_links(driver):
    namelist = []
    link_list = []
    products = driver.find_elements(By.CLASS_NAME, "product-card__title")
    for product in products:
        name=product.find_element(By.CLASS_NAME, "link").text.strip()
        link=product.find_element(By.TAG_NAME, "a").get_attribute("href")
        namelist.append(name)
        link_list.append(link)


for i in range(1,10):
        driver.get(f"https://www.coolblue.be/fr/telephones-portables?redirect=smartphone&page={i}")
        get_links(driver)

data=pd.DataFrame({"name":namelist,"link":link_list})
# Each link at a time, scrap phones' characteristics
# What info do we need ?
# Price -


def get_dev_info(driver):
    tables= driver.find_elements(By.TAG_NAME,"tbody")
    varnamelist = []
    varvaluelist = []
    price=driver.find_element(By.CLASS_NAME,"main-information-1j52iy6").text.strip()
    varnamelist.append("price")
    varvaluelist.append(price)
    for table in tables:

        rows=table.find_elements(By.TAG_NAME, "tr")

        for row in rows:
            varname=row.find_element(By.TAG_NAME, "th").text
            varvalue=row.find_element(By.CLASS_NAME, "css-7wsoqo").text
            varnamelist.append(varname)
            varvaluelist.append(varvalue)
    dataphone=pd.DataFrame({"Variable":varnamelist,"Value":varvaluelist})
    return(dataphone)


get_dev_info(driver)

df=[]
for i in range(1,len(data)):
    driver.get(data['link'].iloc[i])
    model=data['name'].iloc[i]
    try:
        dataphone=get_dev_info(driver)
        print(f"{model} has been scraped")
    except:
        print("An exception occurred")

    dataphone["model"]=model
    df.append(dataphone)

final_df = pd.concat(df)

final_df.loc[final_df["Variable"] == "Produit"].shape

final_df.to_csv("dataphonesB.csv")
