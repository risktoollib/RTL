// Import Playwright's Firefox module and Cheerio
const { firefox } = require("playwright");
const cheerio = require("cheerio");
const fs = require("fs");

// URL is taken from the command line arguments
const url = process.argv[2];

// Asynchronous function to perform web scraping
(async () => {
  // Launch a headless Firefox browser
  const browser = await firefox.launch({ headless: true });
  // Open a new page in the browser
  const page = await browser.newPage();

  // Navigate to the specified URL
  await page.goto(url);
  // Wait until network activity is idle
  await page.waitForLoadState("networkidle");

  // Get the HTML content of the page
  const content = await page.content();
  // Use Cheerio to load the HTML content for parsing
  const $ = cheerio.load(content);

  // Initialize an array to store the scraped data
  let dataFrame = [];

  // Select the '.spec-item single' and '.spec-item multi' elements and iterate over them
  $(".spec-item.single, .spec-item.multi").each((index, element) => {
    // Extract the header, typically in a 'span' or 'div' element
    const header = $(element).find("span, div").first().text().trim();
    console.log(header);

    // Extract the description, typically following the header element
    const description = $(element).find("span, div").eq(1).text().trim();
    console.log(description);

    // Only add to the array if both header and description are not empty
    if (header && description) {
      dataFrame.push({ Specification: header, Description: description });
    }
  });

  // Log the scraped data in JSON format
  console.log(JSON.stringify(dataFrame));
  // You can also write the data to a file if needed
  const fs = require("fs");
  fs.writeFileSync("cme-specifications.json", JSON.stringify(dataFrame));
  // Close the browser
  await browser.close();
})();
