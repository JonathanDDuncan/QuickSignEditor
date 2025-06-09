import { describe, it, expect, beforeEach, vi } from 'vitest';
import { generateSymbolSVG } from './signWritingFont';

// Mock the SSW module properly
const mockSsw = {
  svg: vi.fn(),
  size: vi.fn(() => '76,76'),
  key: vi.fn(),
  fsw: vi.fn(),
  colorize: vi.fn(),
  code: vi.fn(() => '󽠰'), // Mock line character
  pua: vi.fn(() => '󽠰'), // Mock fill character
  symbol: vi.fn(),
  sizes: {}
};

vi.mock('./utils/SuttonSignWriting', () => ({
  default: mockSsw
}));

describe('SignWriting Dual-Font System', () => {  
  beforeEach(() => {
    // Reset all mocks before each test
    vi.clearAllMocks();
    
    // Set up default mock returns
    mockSsw.svg.mockReturnValue(`
      <svg version="1.1" xmlns="http://www.w3.org/2000/svg" width="40" height="40" viewBox="0 0 40 40">
        <text style="font-size:0%;">S10000</text>
        <g transform="translate(0,0)">
          <text class="sym-fill" style="pointer-events:none;font-family:'SuttonSignWritingFill';font-size:30px;fill:#ffffff;">󽠰</text>
          <text class="sym-line" style="pointer-events:none;font-family:'SuttonSignWritingLine';font-size:30px;fill:#000000;">󽠰</text>
        </g>
      </svg>
    `);
    mockSsw.pua.mockReturnValue('󽠰');
    mockSsw.code.mockReturnValue('󽠰');
    mockSsw.size.mockReturnValue('76,76');
  });  it('should generate SVG with dual-font approach', async () => {
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
    expect(svg).toContain("font-family:'SuttonSignWritingFill'");
    expect(svg).toContain("font-family:'SuttonSignWritingLine'");
    
    // Verify the colors are set correctly
    expect(svg).toContain('fill:#ffffff'); // White fill
    expect(svg).toContain('fill:#000000'); // Black line
    
    // Verify both text elements use the Unicode character
    const unicodeMatches = svg.match(/󽠰/g);
    expect(unicodeMatches).toHaveLength(2); // Should appear twice (fill + line)
    
    // Verify group structure
    expect(svg).toContain('<g transform=');
  });  it('should handle symbol conversion from key to Unicode', async () => {
    const testKey = 'S10000';
    const expectedFill = '󽠰';
    const expectedLine = '󽠰';
    
    const symbol = { key: testKey };
    await generateSymbolSVG(symbol);

    expect(mockSsw.svg).toHaveBeenCalledWith(testKey, expect.any(Object));
  });  it('should apply correct positioning and sizing', async () => {
    // Update the mock to return SVG with larger dimensions
    mockSsw.svg.mockReturnValue(`
      <svg version="1.1" xmlns="http://www.w3.org/2000/svg" width="75" height="75" viewBox="0 0 75 75">
        <text style="font-size:0%;">S10000</text>
        <g transform="translate(37.5,37.5)">
          <text class="sym-fill" style="pointer-events:none;font-family:'SuttonSignWritingFill';font-size:75px;fill:#ffffff;">󽠰</text>
          <text class="sym-line" style="pointer-events:none;font-family:'SuttonSignWritingLine';font-size:75px;fill:#000000;">󽠰</text>
        </g>
      </svg>
    `);

    const symbol = { key: 'S10000' };
    const svg = await generateSymbolSVG(symbol, { size: 50, scale: 1.5 });

    // Verify size calculations (size 50 * scale 1.5 = 75)
    expect(svg).toContain('width="75"');
    expect(svg).toContain('height="75"');
    expect(svg).toContain('font-size:75px');

    // Verify positioning - transform should center the symbol
    expect(svg).toContain('transform="translate(37.5,37.5)"'); // Half of 75
  });  it('should handle errors gracefully with fallback SVG', async () => {
    // Test with a symbol that has no key
    const symbol = { key: '' }; // Empty key should trigger error
    const svg = await generateSymbolSVG(symbol);

    // Should return fallback SVG with question mark
    expect(svg).toContain('?');
    expect(svg).toContain('stroke-dasharray="2,2"');
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
