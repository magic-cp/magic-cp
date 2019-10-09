import sys
from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC

url = sys.argv[1]

driver = webdriver.Chrome()
driver.get(url)
try:
    driver.find_element_by_class_name('click-to-view-tests').click()
    element = WebDriverWait(driver, 15).until(
        EC.presence_of_element_located((By.XPATH, "//div[@class='tests-placeholder']/div[3]"))
    )
finally:
    inp = driver.find_elements_by_class_name('input-view')[-1].find_element_by_class_name('input').text
    ans = driver.find_elements_by_class_name('answer-view')[-1].find_element_by_class_name('answer').text
    print(inp)
    print('##ENDOFINPUT##')
    print(ans)

    driver.close()

