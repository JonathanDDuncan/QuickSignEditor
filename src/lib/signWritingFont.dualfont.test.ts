import { describe, it, expect, beforeEach, afterEach, vi } from 'vitest';
import { loadSignWritingFonts, generateSymbolSVG, generateFSWSVG } from './signWritingFont';

// Mock the global window.ssw object
const mockSsw = {
  svg: vi.fn(),
  size: vi.fn(),
  key: vi.fn(),
  fsw: vi.fn(),
  colorize: vi.fn(),
  code: vi.fn(),
  pua: vi.fn(),
};

// Mock window object for browser environment
Object.defineProperty(window, 'ssw', {
  value: mockSsw,
  writable: true,
});

describe('SignWriting Dual-Font Implementation', () => {
  beforeEach(() => {
    vi.clearAllMocks();
    // Reset mock implementations
    mockSsw.svg.mockImplementation(() => '<svg><text>Mock SVG</text></svg>');
    mockSsw.size.mockImplementation(() => '30,30');
    mockSsw.key.mockImplementation((text) => text);
    mockSsw.pua.mockImplementation(() => 'S10000');
  });

  afterEach(() => {
    vi.clearAllMocks();
  });

  describe('Font Loading', () => {
    it('should load SignWriting fonts successfully', async () => {
      const result = await loadSignWritingFonts();
      expect(result).toBe(true);
    });

    it('should handle missing ssw library gracefully', async () => {
      // Temporarily remove ssw
      const originalSsw = window.ssw;
      delete (window as any).ssw;
      
      try {
        const result = await loadSignWritingFonts();
        expect(result).toBe(false);
      } finally {
        window.ssw = originalSsw;
      }
    });
  });

  describe('Symbol SVG Generation', () => {
    it('should generate dual-font SVG for valid symbol', async () => {
      const testSymbol = {
        key: 'S10000',
        width: 40,
        height: 40,
        size: 1
      };

      mockSsw.svg.mockReturnValue(
        '<svg><text class="sym-fill" style="font-family:\'SuttonSignWritingFill\';">􍀀</text>' +
        '<text class="sym-line" style="font-family:\'SuttonSignWritingLine\';">󽀀</text></svg>'
      );

      const result = await generateSymbolSVG(testSymbol, { size: 60 });
      
      expect(result).toContain('sym-fill');
      expect(result).toContain('sym-line');
      expect(result).toContain('SuttonSignWritingFill');
      expect(result).toContain('SuttonSignWritingLine');
      expect(mockSsw.svg).toHaveBeenCalledWith('S10000', expect.objectContaining({
        size: 60
      }));
    });

    it('should handle invalid symbol keys gracefully', async () => {
      const invalidSymbol = {
        key: 'INVALID',
        width: 40,
        height: 40,
        size: 1
      };

      mockSsw.svg.mockReturnValue('');

      const result = await generateSymbolSVG(invalidSymbol);
      expect(result).toBe('');
      expect(mockSsw.svg).toHaveBeenCalledWith('INVALID', expect.any(Object));
    });

    it('should apply custom styling options', async () => {
      const testSymbol = {
        key: 'S20000',
        width: 50,
        height: 50,
        size: 2
      };

      const options = {
        size: 80,
        fill: '#ff0000',
        line: '#0000ff'
      };

      await generateSymbolSVG(testSymbol, options);
      
      expect(mockSsw.svg).toHaveBeenCalledWith('S20000', expect.objectContaining({
        size: 80,
        fill: '#ff0000',
        line: '#0000ff'
      }));
    });
  });

  describe('FSW String Generation', () => {
    it('should generate dual-font SVG for valid FSW string', async () => {
      const testFSW = 'AS20310S26b02S33100M521x547S33100482x483S20310506x500S26b02503x520';
      
      mockSsw.svg.mockReturnValue(
        '<svg version="1.1" xmlns="http://www.w3.org/2000/svg" width="39" height="64" viewBox="482 483 39 64">' +
        '<text style="font-size:0%;">AS20310S26b02S33100M521x547S33100482x483S20310506x500S26b02503x520</text>' +
        '<g transform="translate(482,483)">' +
        '<text class="sym-fill" style="font-family:\'SuttonSignWritingFill\';fill:white;">􍉡</text>' +
        '<text class="sym-line" style="font-family:\'SuttonSignWritingLine\';fill:black;">󽉡</text>' +
        '</g>' +
        '</svg>'
      );

      const result = await generateFSWSVG(testFSW);
      
      expect(result).toContain('sym-fill');
      expect(result).toContain('sym-line');
      expect(result).toContain('SuttonSignWritingFill');
      expect(result).toContain('SuttonSignWritingLine');
      expect(result).toContain('fill:white');
      expect(result).toContain('fill:black');
      expect(mockSsw.svg).toHaveBeenCalledWith(testFSW, expect.any(Object));
    });

    it('should handle simple FSW symbols', async () => {
      const simpleFSW = 'S10000482x483';
      
      mockSsw.svg.mockReturnValue(
        '<svg><text class="sym-fill">􍀀</text><text class="sym-line">󽀀</text></svg>'
      );

      const result = await generateFSWSVG(simpleFSW);
      expect(result).toContain('sym-fill');
      expect(result).toContain('sym-line');
      expect(mockSsw.svg).toHaveBeenCalledWith(simpleFSW, expect.any(Object));
    });

    it('should apply custom options to FSW generation', async () => {
      const testFSW = 'M521x547S33100482x483';
      const options = {
        scale: 2,
        fill: '#ffffff',
        line: '#000000'
      };

      await generateFSWSVG(testFSW, options);
      
      expect(mockSsw.svg).toHaveBeenCalledWith(testFSW, expect.objectContaining({
        scale: 2,
        fill: '#ffffff',
        line: '#000000'
      }));
    });

    it('should handle empty FSW strings gracefully', async () => {
      mockSsw.svg.mockReturnValue('');

      const result = await generateFSWSVG('');
      expect(result).toBe('');
    });

    it('should handle malformed FSW strings', async () => {
      const malformedFSW = 'INVALID_FSW_STRING';
      
      mockSsw.svg.mockReturnValue('');

      const result = await generateFSWSVG(malformedFSW);
      expect(result).toBe('');
      expect(mockSsw.svg).toHaveBeenCalledWith(malformedFSW, expect.any(Object));
    });
  });

  describe('Dual-Font Structure Validation', () => {
    it('should verify dual-font SVG structure contains required elements', async () => {
      const testSymbol = {
        key: 'S30000',
        width: 40,
        height: 40,
        size: 1
      };

      mockSsw.svg.mockReturnValue(`
        <svg xmlns="http://www.w3.org/2000/svg">
          <g>
            <text class="sym-fill" style="font-family:'SuttonSignWritingFill';fill:white;">􍄅</text>
            <text class="sym-line" style="font-family:'SuttonSignWritingLine';fill:black;">󽄅</text>
          </g>
        </svg>
      `);

      const result = await generateSymbolSVG(testSymbol);
      
      // Check for dual-font structure
      expect(result).toMatch(/class="sym-fill"/);
      expect(result).toMatch(/class="sym-line"/);
      expect(result).toMatch(/font-family:['"']SuttonSignWritingFill['"']/);
      expect(result).toMatch(/font-family:['"']SuttonSignWritingLine['"']/);
      expect(result).toMatch(/fill:\s*white/);
      expect(result).toMatch(/fill:\s*black/);
    });

    it('should validate proper layering order (fill first, then line)', async () => {
      const testFSW = 'S10000482x483';
      
      mockSsw.svg.mockReturnValue(`
        <svg>
          <g>
            <text class="sym-fill" style="font-family:'SuttonSignWritingFill';">􍀀</text>
            <text class="sym-line" style="font-family:'SuttonSignWritingLine';">󽀀</text>
          </g>
        </svg>
      `);

      const result = await generateFSWSVG(testFSW);
      
      // Check that fill comes before line in the SVG structure
      const fillIndex = result.indexOf('sym-fill');
      const lineIndex = result.indexOf('sym-line');
      
      expect(fillIndex).toBeGreaterThan(-1);
      expect(lineIndex).toBeGreaterThan(-1);
      expect(fillIndex).toBeLessThan(lineIndex);
    });
  });

  describe('Error Handling', () => {
    it('should handle SSW library errors gracefully', async () => {
      mockSsw.svg.mockImplementation(() => {
        throw new Error('SSW library error');
      });

      const testSymbol = {
        key: 'S10000',
        width: 40,
        height: 40,
        size: 1
      };

      const result = await generateSymbolSVG(testSymbol);
      expect(result).toBe('');
    });

    it('should handle network timeouts during font loading', async () => {
      // Simulate timeout by removing ssw temporarily
      const originalSsw = window.ssw;
      delete (window as any).ssw;
      
      try {
        const startTime = Date.now();
        const result = await loadSignWritingFonts();
        const endTime = Date.now();
        
        expect(result).toBe(false);
        expect(endTime - startTime).toBeGreaterThan(4000); // Should timeout after ~5 seconds
      } finally {
        window.ssw = originalSsw;
      }
    });

    it('should handle missing font options gracefully', async () => {
      const testSymbol = {
        key: 'S10000',
        width: 40,
        height: 40,
        size: 1
      };

      // Test with undefined options
      await generateSymbolSVG(testSymbol, undefined);
      expect(mockSsw.svg).toHaveBeenCalledWith('S10000', expect.any(Object));

      // Test with empty options
      await generateSymbolSVG(testSymbol, {});
      expect(mockSsw.svg).toHaveBeenCalledWith('S10000', expect.any(Object));
    });
  });

  describe('Performance', () => {
    it('should generate SVG within acceptable time limits', async () => {
      const testSymbol = {
        key: 'S10000',
        width: 40,
        height: 40,
        size: 1
      };

      const startTime = Date.now();
      await generateSymbolSVG(testSymbol);
      const endTime = Date.now();

      // Should complete within 100ms (generous for testing)
      expect(endTime - startTime).toBeLessThan(100);
    });

    it('should handle multiple concurrent generations', async () => {
      const symbols = [
        { key: 'S10000', width: 40, height: 40, size: 1 },
        { key: 'S20000', width: 40, height: 40, size: 1 },
        { key: 'S30000', width: 40, height: 40, size: 1 }
      ];

      const startTime = Date.now();
      const promises = symbols.map(symbol => generateSymbolSVG(symbol));
      await Promise.all(promises);
      const endTime = Date.now();

      // Should complete all within reasonable time
      expect(endTime - startTime).toBeLessThan(500);
      expect(mockSsw.svg).toHaveBeenCalledTimes(3);
    });
  });

  describe('Integration with Official SSW Library', () => {
    it('should pass correct parameters to ssw.svg for symbols', async () => {
      const testSymbol = {
        key: 'S12345',
        width: 60,
        height: 80,
        size: 2,
        nwcolor: '#ff0000',
        nbcolor: '#0000ff'
      };

      await generateSymbolSVG(testSymbol, { size: 100 });

      expect(mockSsw.svg).toHaveBeenCalledWith(
        'S12345',
        expect.objectContaining({
          size: 100
        })
      );
    });

    it('should pass correct parameters to ssw.svg for FSW strings', async () => {
      const testFSW = 'AS20310S26b02S33100M521x547S33100482x483S20310506x500S26b02503x520';
      const options = {
        scale: 1.5,
        size: 40,
        line: '#333333',
        fill: '#ffffff'
      };

      await generateFSWSVG(testFSW, options);

      expect(mockSsw.svg).toHaveBeenCalledWith(
        testFSW,
        expect.objectContaining({
          scale: 1.5,
          size: 40,
          line: '#333333',
          fill: '#ffffff'
        })
      );
    });

    it('should maintain compatibility with existing SSW library features', async () => {
      // Test that all expected SSW methods are accessible
      expect(typeof window.ssw.svg).toBe('function');
      expect(typeof window.ssw.size).toBe('function');
      expect(typeof window.ssw.key).toBe('function');
      expect(typeof window.ssw.fsw).toBe('function');
      expect(typeof window.ssw.colorize).toBe('function');
      expect(typeof window.ssw.code).toBe('function');
      expect(typeof window.ssw.pua).toBe('function');
    });
  });
});
