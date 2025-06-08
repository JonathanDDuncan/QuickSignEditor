/**
 * SignWriting Symbol Data Loader
 * 
 * This file provides basic symbol size data for common SignWriting symbols
 * to populate the ssw.sizes object when the library is loaded.
 */

// Common SignWriting symbol size mappings (key -> "width x height")
const SYMBOL_SIZES = {
  // Hand symbols (most common)
  'S10000': '30x30',  // Index finger pointing up
  'S10001': '30x30',  // Index finger pointing up, rotated
  'S10002': '30x30',  // Index finger pointing up, different fill
  'S10003': '30x30',  // Index finger pointing up, different rotation
  'S10004': '30x30',  // Index finger pointing up, different fill/rotation
  'S10005': '30x30',  // Index finger pointing up, different rotation
  'S10006': '30x30',  // Index finger pointing up, different fill/rotation
  'S10007': '30x30',  // Index finger pointing up, different rotation
  'S10008': '30x30',  // Index finger pointing right
  'S10009': '30x30',  // Index finger pointing right, different fill
  'S1000a': '30x30',  // Index finger pointing right, different rotation
  'S1000b': '30x30',  // Index finger pointing right, different fill/rotation
  'S1000c': '30x30',  // Index finger pointing down
  'S1000d': '30x30',  // Index finger pointing down, different fill
  'S1000e': '30x30',  // Index finger pointing down, different rotation
  'S1000f': '30x30',  // Index finger pointing down, different fill/rotation
  
  // More hand configurations
  'S10010': '30x30',  // Index finger pointing left
  'S10011': '30x30',  // Index finger pointing left, different fill
  'S10012': '30x30',  // Index finger pointing left, different rotation
  'S10013': '30x30',  // Index finger pointing left, different fill/rotation
  'S10014': '30x30',  // Index finger pointing diagonally
  'S10015': '30x30',  // Index finger pointing diagonally, different fill
  'S10016': '30x30',  // Index finger pointing diagonally, different rotation
  'S10017': '30x30',  // Index finger pointing diagonally, different fill/rotation
  'S10018': '30x30',  // Index finger pointing diagonally down
  'S10019': '30x30',  // Index finger pointing diagonally down, different fill
  'S1001a': '30x30',  // Index finger pointing diagonally down, different rotation
  'S1001b': '30x30',  // Index finger pointing diagonally down, different fill/rotation
  'S1001c': '30x30',  // Index finger pointing diagonally left
  'S1001d': '30x30',  // Index finger pointing diagonally left, different fill
  'S1001e': '30x30',  // Index finger pointing diagonally left, different rotation
  'S1001f': '30x30',  // Index finger pointing diagonally left, different fill/rotation
  
  // Simplified test symbols
  'S100': '30x30',    // Simplified key for testing
  'S101': '30x30',    // Simplified key for testing
  'S102': '30x30',    // Simplified key for testing
  'S103': '30x30',    // Simplified key for testing
  
  // Fist symbols
  'S10020': '25x30',  // Closed fist
  'S10021': '25x30',  // Closed fist, different fill
  'S10022': '25x30',  // Closed fist, different rotation
  'S10023': '25x30',  // Closed fist, different fill/rotation
  
  // Multiple finger symbols
  'S10040': '35x30',  // Index and middle fingers
  'S10041': '35x30',  // Index and middle fingers, different fill
  'S10042': '35x30',  // Index and middle fingers, different rotation
  'S10043': '35x30',  // Index and middle fingers, different fill/rotation
  
  // Palm symbols
  'S10060': '40x40',  // Open palm
  'S10061': '40x40',  // Open palm, different fill
  'S10062': '40x40',  // Open palm, different rotation
  'S10063': '40x40',  // Open palm, different fill/rotation
  
  // Movement symbols
  'S20500': '30x15',  // Straight movement
  'S20501': '30x15',  // Straight movement, different fill
  'S20502': '30x15',  // Straight movement, different rotation
  'S20503': '30x15',  // Straight movement, different fill/rotation
  
  // Curved movement
  'S20600': '35x20',  // Curved movement
  'S20601': '35x20',  // Curved movement, different fill
  'S20602': '35x20',  // Curved movement, different rotation
  'S20603': '35x20',  // Curved movement, different fill/rotation
  
  // Contact symbols
  'S25000': '20x20',  // Touch
  'S25001': '20x20',  // Touch, different fill
  'S25002': '20x20',  // Touch, different rotation
  'S25003': '20x20',  // Touch, different fill/rotation
  
  // Face and head symbols
  'S30000': '30x35',  // Head
  'S30001': '30x35',  // Head, different fill
  'S30002': '30x35',  // Head, different rotation
  'S30003': '30x35',  // Head, different fill/rotation
  
  // Eye symbols
  'S30100': '25x15',  // Eye
  'S30101': '25x15',  // Eye, different fill
  'S30102': '25x15',  // Eye, different rotation
  'S30103': '25x15',  // Eye, different fill/rotation
  
  // Mouth symbols
  'S30200': '25x15',  // Mouth
  'S30201': '25x15',  // Mouth, different fill
  'S30202': '25x15',  // Mouth, different rotation
  'S30203': '25x15',  // Mouth, different fill/rotation
};

/**
 * Load symbol size data into the SuttonSignWriting library
 */
function loadSymbolSizes() {
  if (typeof window !== 'undefined' && window.ssw && window.ssw.sizes) {
    console.log('Loading SignWriting symbol size data...');
    
    // Add all predefined symbol sizes
    Object.assign(window.ssw.sizes, SYMBOL_SIZES);
    
    console.log(`Loaded ${Object.keys(SYMBOL_SIZES).length} symbol size definitions`);
    console.log('Sample loaded symbols:', Object.keys(SYMBOL_SIZES).slice(0, 10));
    
    // Verify some key symbols are loaded
    const testSymbols = ['S10000', 'S100', 'S101', 'S102'];
    testSymbols.forEach(symbol => {
      const size = window.ssw.size(symbol);
      console.log(`Symbol ${symbol} size:`, size);
    });
    
    return true;
  } else {
    console.warn('SuttonSignWriting library not available for symbol loading');
    return false;
  }
}

/**
 * Auto-load symbol sizes when this script is included
 */
if (typeof window !== 'undefined') {
  // Try to load immediately if library is available
  if (window.ssw) {
    loadSymbolSizes();
  } else {
    // Wait for library to load
    window.addEventListener('load', () => {
      setTimeout(loadSymbolSizes, 100); // Small delay to ensure library is ready
    });
  }
}

// Export for manual loading if needed
if (typeof module !== 'undefined' && module.exports) {
  module.exports = { SYMBOL_SIZES, loadSymbolSizes };
}
