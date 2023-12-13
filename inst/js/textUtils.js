// Function to clean and escape text for JSON
const cleanText = (text) => {
  // Replace single quotes with escaped single quotes
  // Replace line breaks and tabs with a space
  // Trim the result to remove leading and trailing whitespace
  return text
    .replace(/'/g, "\\'")
    .replace(/\r?\n|\r|\t/g, " ")
    .trim();
};

module.exports = { cleanText };
