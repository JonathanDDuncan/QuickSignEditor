// Debug script to check mannequin state in browser console
// Run this in the browser console of http://localhost:5175/

console.log('=== MANNEQUIN DEBUG CHECK ===');

// Check if global debug functions are available
console.log('1. Global debug functions:');
console.log('   window.debugMannequin:', typeof window.debugMannequin);
console.log('   window.reloadMannequinData:', typeof window.reloadMannequinData);

// Check SSW library - note: migrated to ES6 module imports
console.log('2. SSW Library:');
console.log('   window.ssw:', typeof window.ssw);
console.log('   window.ssw.svg:', typeof window.ssw?.svg);
console.log('   Note: SSW now uses proper ES6 imports instead of window.ssw');

// Test SSW functionality - this should be updated to use the ES6 import in components
if (window.ssw && window.ssw.svg) {
    try {
        const testSvg = window.ssw.svg('S10000', {});
        console.log('   SSW test successful, output length:', testSvg.length);
    } catch (e) {
        console.log('   SSW test failed:', e.message);
        console.log('   Note: Components should use the ES6 import instead of window.ssw');
    }
} else {
    console.log('   SSW library not globally available - use ES6 imports instead');
}

// Check for Svelte app
console.log('3. Svelte App:');
const app = document.getElementById('app');
console.log('   App container found:', !!app);
if (app) {
    console.log('   App container content length:', app.innerHTML.length);
    
    // Look for mannequin section
    const mannequinSection = app.querySelector('[style*="margin-top: 2rem"]');
    console.log('   Mannequin section found:', !!mannequinSection);
    
    if (mannequinSection) {
        console.log('   Mannequin section content:', mannequinSection.innerHTML.substring(0, 200));
    }
}

// Run global debug function if available
if (typeof window.debugMannequin === 'function') {
    console.log('4. Running debugMannequin():');
    window.debugMannequin();
} else {
    console.log('4. debugMannequin() not available');
}

// Try to reload data if available
if (typeof window.reloadMannequinData === 'function') {
    console.log('5. Running reloadMannequinData():');
    window.reloadMannequinData();
} else {
    console.log('5. reloadMannequinData() not available');
}

console.log('=== END DEBUG CHECK ===');
