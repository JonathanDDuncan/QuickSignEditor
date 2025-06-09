// Test script to run in browser console
console.log('=== Testing Mannequin Data Loading ===');

// Test the FSW parsing directly
function testFSWParsing() {
    const complexFSW = "M600x697S33000476x489S2ff00484x488S30a50495x490S30a40516x490S31430498x500S33e10501x520S33110510x511S36d10481x558S36a10498x538S37601546x560S37609463x560S14c10554x555S20500576x542S21600552x542S22800463x558S2f700562x519S22a00470x636S22e00583x587S23800487x631S24200506x631S24b00534x628S28800468x668S29900491x663S2e300534x665S2ed00568x587S2f100448x542S2f500564x655S36b10463x454S36c00493x447S38500534x443S37e0f440x559S29600511x663S38700476x695--Z01,1.48Z02,1.51Z05,1.21Z06,1.45Z08,1.63Z09,1.45Z10,1.6Z11,1.6Z25,0.79Z28,0.79Z29,0.79Z30,0.79";
    
    console.log('FSW string length:', complexFSW.length);
    
    const symbolRegex = /S[0-9a-f]{5}(\d{3})x(\d{3})/gi;
    const symbols = [];
    let match;
    
    while ((match = symbolRegex.exec(complexFSW)) !== null) {
        const key = match[0].substring(0, 6);
        const x = parseInt(match[1]);
        const y = parseInt(match[2]);
        symbols.push({ key, x, y });
        console.log(`Found: ${key} at (${x}, ${y})`);
    }
    
    console.log(`Total symbols found: ${symbols.length}`);
    return symbols;
}

// Run the test
const symbols = testFSWParsing();

// Test if we can trigger the loadManiquinData function
console.log('Attempting to trigger loadManiquinData...');

// This should work if the Svelte app is loaded
if (typeof window !== 'undefined') {
    console.log('Window object available');
    console.log('Available global functions:', Object.keys(window).filter(k => typeof window[k] === 'function').slice(0, 10));
}
