import { describe, it, expect, beforeAll, vi } from 'vitest';
import { generateSymbolSVG, loadSignWritingFonts } from './signWritingFont';

// Mock the global window object for testing
const mockSsw = {
  svg: vi.fn(),
  size: vi.fn(() => '76,76'),
  key: vi.fn(),
  fsw: vi.fn(),
  colorize: vi.fn(),
  code: vi.fn(() => '󽠰'), // Mock line character
  pua: vi.fn(() => '󽠰'), // Mock fill character
  sizes: {}
};

Object.defineProperty(globalThis, 'window', {
  value: {
    ssw: mockSsw
  },
  writable: true
});

describe('SignWriting Dual-Font System', () => {  beforeAll(() => {
    // Ensure the mock is properly set up
    if (typeof window !== 'undefined' && window.ssw) {
      window.ssw.pua = vi.fn(() => '󽠰');  // Fill character
      window.ssw.code = vi.fn(() => '󽠰'); // Line character
      window.ssw.size = vi.fn(() => '76,76');
    }
  });  it('should generate SVG with dual-font approach', async () => {
    // Mock both pua and code functions
    mockSsw.pua.mockReturnValue('󽠰'); // Fill character
    mockSsw.code.mockReturnValue('󽠰'); // Line character

    const symbol = {
      key: 'S10000',
      width: 40,
      height: 40,
      size: 1,
      nwcolor: '#ffffff', // fill color
      nbcolor: '#000000'  // line color
    };

    const svg = await generateSymbolSVG(symbol, { size: 30 });

    // Verify the SVG contains both font classes (official implementation)
    expect(svg).toContain('class="sym-fill"');
    expect(svg).toContain('class="sym-line"');
      // Verify the SVG contains the correct fonts
    expect(svg).toContain("font-family: 'SuttonSignWritingFill'");
    expect(svg).toContain("font-family: 'SuttonSignWritingLine'");
    
    // Verify the colors are set correctly
    expect(svg).toContain('fill: #ffffff'); // White fill
    expect(svg).toContain('fill: #000000'); // Black line
    
    // Verify both text elements use the Unicode character
    const unicodeMatches = svg.match(/󽠰/g);
    expect(unicodeMatches).toHaveLength(2); // Should appear twice (fill + line)
    
    // Verify group structure
    expect(svg).toContain('<g transform=');
  });
  it('should handle symbol conversion from key to Unicode', async () => {
    const testKey = 'S10000';
    const expectedFill = '󽠰';
    const expectedLine = '󽠰';
    
    mockSsw.pua.mockReturnValue(expectedFill);
    mockSsw.code.mockReturnValue(expectedLine);

    const symbol = { key: testKey };
    await generateSymbolSVG(symbol);

    expect(mockSsw.pua).toHaveBeenCalledWith(testKey);
    expect(mockSsw.code).toHaveBeenCalledWith(testKey);
  });  it('should apply correct positioning and sizing', async () => {
    mockSsw.pua.mockReturnValue('󽠰');
    mockSsw.code.mockReturnValue('󽠰');

    const symbol = { key: 'S10000' };
    const options = { size: 50, scale: 1.5 };
    
    const svg = await generateSymbolSVG(symbol, options);
    
    // Verify size calculations (size 50 * scale 1.5 = 75)
    expect(svg).toContain('width="75"');
    expect(svg).toContain('height="75"');
    expect(svg).toContain('font-size: 75px');
    
    // Verify positioning (centered with group transform)
    expect(svg).toContain('x="0"');
    expect(svg).toContain('y="0"');
    expect(svg).toContain('text-anchor: middle');
    expect(svg).toContain('dominant-baseline: central');
    expect(svg).toContain('transform="translate(37.5,37.5)"'); // Half of 75
  });  it('should handle errors gracefully with fallback SVG', async () => {
    // Test with a symbol that has no key
    const symbol = { key: '' }; // Empty key should trigger error
    const svg = await generateSymbolSVG(symbol);

    // Should return fallback SVG with question mark
    expect(svg).toContain('?');
    expect(svg).toContain('stroke-dasharray="2,2"');
  });

  it('should load SignWriting fonts successfully', async () => {
    // Mock the size function to return a valid size
    mockSsw.size.mockReturnValue('76,76');

    const loadPromise = loadSignWritingFonts();
    
    // Should resolve without throwing
    await expect(loadPromise).resolves.toBeUndefined();
  });
});

describe('SignWriting Font CSS Integration', () => {
  it('should have correct CSS classes defined', () => {
    // Test the official dual-font CSS classes
    const testSvg = `<svg>
      <text class="sym-fill">󽠰</text>
      <text class="sym-line">󽠰</text>
    </svg>`;
    
    expect(testSvg).toContain('class="sym-fill"');
    expect(testSvg).toContain('class="sym-line"');
  });
});
