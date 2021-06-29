import sys
from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
from selenium.webdriver.chrome.options import Options

url = sys.argv[1]

chrome_options = Options()
chrome_options.add_argument('--headless')
chrome_options.add_argument('--no-sandbox')
chrome_options.add_argument('--disable-dev-shm-usage')

driver = webdriver.Chrome(options=chrome_options)
driver.get(url)
try:
    driver.find_element_by_class_name('click-to-view-tests').click()
    element = WebDriverWait(driver, 15).until(
        EC.presence_of_element_located((By.XPATH, "//div[@class='tests-placeholder']/div[3]"))
    )
finally:
    inputs = driver.find_elements_by_css_selector('.input-view .input')[1:]
    outputs = driver.find_elements_by_css_selector('.answer-view .answer')[1:]
    elems = list(zip(inputs, outputs))
    for i in range(len(elems)):
        inp, ans = elems[i]
        print(inp.text)
        print('##ENDOFINPUT##')
        print(ans.text)

        if i != len(elems) - 1:
            print('##ENDOFCASE##')

    driver.close()

