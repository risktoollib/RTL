const { firefox } = require("playwright");
const fs = require("fs");

(async () => {
  const browser = await firefox.launch();
  const page = await browser.newPage();

  // Taking URL as the first input argument
  const url = process.argv[2];
  if (!url) {
    console.log("Please provide a URL as the first argument.");
    await browser.close();
    return;
  }

  // Taking the option index as the second input argument
  const optionIndex = parseInt(process.argv[3]);
  if (isNaN(optionIndex)) {
    console.log(
      "Please provide the option index (a number) as the second argument."
    );
    await browser.close();
    return;
  }

  await page.goto(url);
  await page.waitForLoadState("networkidle");

  // Known selector for the dropdown
  const selector = ".selectBox";

  // Click the dropdown to show options
  const elem = await page.$(selector);
  await elem.click();
  await page.waitForSelector(`${selector} option`);
  const options = await page.$$(`${selector} option`);

  if (optionIndex >= 0 && optionIndex < options.length) {
    await options[optionIndex].click();
  } else {
    console.log(`Option at index ${optionIndex} not found.`);
  }

  await page.waitForLoadState("networkidle");

  // Extracting data from the div with class 'curr_table'
  const tableData = await page.$$eval(".curr_table tr", (rows) => {
    return rows.map((row) => {
      const cells = row.querySelectorAll("th, td");
      return Array.from(cells, (cell) => cell.textContent.trim());
    });
  });

  // Close the browser
  await browser.close();

  // Transform the array of rows into a more structured JSON object
  const headers = tableData[0]; // Assuming the first row contains headers
  const jsonData = tableData.slice(1).map((row) => {
    return headers.reduce((obj, header, index) => {
      obj[header] = row[index];
      return obj;
    }, {});
  });

  // Save the JSON data to a file
  const jsonFileName = "table_data.json";
  fs.writeFileSync(jsonFileName, JSON.stringify(jsonData, null, 2));
  console.log(`Data saved to ${jsonFileName}`);
})();
