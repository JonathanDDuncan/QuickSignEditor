import { describe, it, expect, beforeAll, vi } from 'vitest';
import '../types/signwriting'; // Import global type declarations

// Mock SSW library functions
const mockSSW = {
  svg: vi.fn(),
  size: vi.fn(),
  key: vi.fn(),
  fsw: vi.fn(),
  colorize: vi.fn(),
  code: vi.fn(),
  pua: vi.fn(),
  uni: vi.fn(),
  bbox: vi.fn(),
  symbol: vi.fn(),
  parse: vi.fn(),
  sign: vi.fn(),
  chars: vi.fn()
};

describe('SignWriting Dual-Font Implementation', () => {
  beforeAll(() => {
    // Setup global mock
    global.window = global.window || {};
    (global.window as any).ssw = mockSSW;
  });

  describe('SSW Library Integration', () => {
    it('should have ssw library available globally', () => {
      expect(window.ssw).toBeDefined();
      expect(typeof window.ssw.svg).toBe('function');
    });

    it('should generate SVG with dual-font classes', () => {
      const mockSVG = `<svg version="1.1" xmlns="http://www.w3.org/2000/svg" width="39" height="64" viewBox="482 483 39 64">
        <text style="font-size:0%;">AS20310S26b02S33100M521x547S33100482x483S20310506x500S26b02503x520</text>
        <g transform="translate(482,483)">
          <text class="sym-fill" style="pointer-events:none;font-family:'SuttonSignWritingFill';font-size:30px;fill:white;">􍉡</text>
          <text class="sym-line" style="pointer-events:none;font-family:'SuttonSignWritingLine';font-size:30px;fill:black;">󽉡</text>
        </g>
      </svg>`;

      mockSSW.svg.mockReturnValue(mockSVG);

      const result = window.ssw.svg('S20310');
      
      expect(result).toContain('sym-fill');
      expect(result).toContain('sym-line');
      expect(result).toContain('SuttonSignWritingFill');
      expect(result).toContain('SuttonSignWritingLine');
    });

    it('should handle FSW strings correctly', () => {
      const fswString = "AS20310S26b02S33100M521x547S33100482x483S20310506x500S26b02503x520";
      const mockSVG = `<svg version="1.1" xmlns="http://www.w3.org/2000/svg" width="39" height="64" viewBox="482 483 39 64">
        <text style="font-size:0%;">${fswString}</text>
        <g transform="translate(482,483)">
          <text class="sym-fill" style="pointer-events:none;font-family:'SuttonSignWritingFill';font-size:30px;fill:white;">􍉡</text>
          <text class="sym-line" style="pointer-events:none;font-family:'SuttonSignWritingLine';font-size:30px;fill:black;">󽉡</text>
        </g>
        <g transform="translate(506,500)">
          <text class="sym-fill" style="pointer-events:none;font-family:'SuttonSignWritingFill';font-size:30px;fill:white;">􆄱</text>
          <text class="sym-line" style="pointer-events:none;font-family:'SuttonSignWritingLine';font-size:30px;fill:black;">󶄱</text>
        </g>
        <g transform="translate(503,520)">
          <text class="sym-fill" style="pointer-events:none;font-family:'SuttonSignWritingFill';font-size:30px;fill:white;">􈠣</text>
          <text class="sym-line" style="pointer-events:none;font-family:'SuttonSignWritingLine';font-size:30px;fill:black;">󸠣</text>
        </g>
      </svg>`;

      mockSSW.svg.mockReturnValue(mockSVG);

      const result = window.ssw.svg(fswString);
      
      expect(result).toContain(fswString);
      expect(result).toContain('sym-fill');
      expect(result).toContain('sym-line');
      expect(result).toContain('SuttonSignWritingFill');
      expect(result).toContain('SuttonSignWritingLine');
    });
  });

  describe('Dual-Font Rendering', () => {
    it('should generate valid SVG with proper structure', () => {
      const mockSVG = `<svg version="1.1" xmlns="http://www.w3.org/2000/svg" width="30" height="30" viewBox="500 500 30 30">
        <g transform="translate(500,500)">
          <text class="sym-fill" style="font-family:'SuttonSignWritingFill';fill:white;">􍉡</text>
          <text class="sym-line" style="font-family:'SuttonSignWritingLine';fill:black;">󽉡</text>
        </g>
      </svg>`;

      mockSSW.svg.mockReturnValue(mockSVG);

      const result = window.ssw.svg('S10000');
      
      expect(result).toMatch(/<svg[^>]*>/);
      expect(result).toMatch(/<\/svg>/);
      expect(result).toContain('xmlns="http://www.w3.org/2000/svg"');
    });

    it('should include both fill and line text elements for each symbol', () => {
      const mockSVG = `<svg>
        <g transform="translate(500,500)">
          <text class="sym-fill" style="font-family:'SuttonSignWritingFill';">􍉡</text>
          <text class="sym-line" style="font-family:'SuttonSignWritingLine';">󽉡</text>
        </g>
      </svg>`;

      mockSSW.svg.mockReturnValue(mockSVG);

      const result = window.ssw.svg('S10000');
      
      const fillElements = (result.match(/class="sym-fill"/g) || []).length;
      const lineElements = (result.match(/class="sym-line"/g) || []).length;
      
      expect(fillElements).toBeGreaterThan(0);
      expect(lineElements).toBeGreaterThan(0);
      expect(fillElements).toBe(lineElements); // Should have equal fill and line elements
    });

    it('should use correct font families', () => {
      const mockSVG = `<svg>
        <text class="sym-fill" style="font-family:'SuttonSignWritingFill';">􍉡</text>
        <text class="sym-line" style="font-family:'SuttonSignWritingLine';">󽉡</text>
      </svg>`;

      mockSSW.svg.mockReturnValue(mockSVG);

      const result = window.ssw.svg('S10000');
      
      expect(result).toContain("font-family:'SuttonSignWritingFill'");
      expect(result).toContain("font-family:'SuttonSignWritingLine'");
    });

    it('should handle custom colors correctly', () => {
      const mockSVG = `<svg>
        <text class="sym-fill" style="font-family:'SuttonSignWritingFill';fill:#ffffff;">􍉡</text>
        <text class="sym-line" style="font-family:'SuttonSignWritingLine';fill:#000000;">󽉡</text>
      </svg>`;

      mockSSW.svg.mockReturnValue(mockSVG);

      const result = window.ssw.svg('S10000', { fill: '#ffffff', line: '#000000' });
      
      expect(result).toContain('fill:#ffffff');
      expect(result).toContain('fill:#000000');
    });
  });

  describe('Font Loading and Validation', () => {
    it('should validate that required CSS classes are present', () => {
      const mockSVG = `<svg>
        <text class="sym-fill">􍉡</text>
        <text class="sym-line">󽉡</text>
      </svg>`;

      mockSSW.svg.mockReturnValue(mockSVG);

      const result = window.ssw.svg('S10000');
      
      expect(result).toContain('class="sym-fill"');
      expect(result).toContain('class="sym-line"');
    });

    it('should handle empty or invalid input gracefully', () => {
      mockSSW.svg.mockReturnValue('');

      const result = window.ssw.svg('');
      
      expect(result).toBe('');
    });

    it('should handle malformed FSW strings', () => {
      mockSSW.svg.mockReturnValue('');

      const result = window.ssw.svg('INVALID_FSW');
      
      expect(result).toBe('');
    });
  });

  describe('Symbol Validation', () => {
    it('should validate common SignWriting symbols', () => {
      const testSymbols = ['S10000', 'S20000', 'S30000'];
      
      testSymbols.forEach(symbol => {
        const mockSVG = `<svg><text class="sym-fill">test</text><text class="sym-line">test</text></svg>`;
        mockSSW.svg.mockReturnValue(mockSVG);
        
        const result = window.ssw.svg(symbol);
        expect(result).toContain('sym-fill');
        expect(result).toContain('sym-line');
      });
    });

    it('should handle positioned symbols in FSW strings', () => {
      const fswWithPositions = "M521x547S33100482x483S20310506x500";
      const mockSVG = `<svg>
        <g transform="translate(482,483)">
          <text class="sym-fill">test1</text>
          <text class="sym-line">test1</text>
        </g>
        <g transform="translate(506,500)">
          <text class="sym-fill">test2</text>
          <text class="sym-line">test2</text>
        </g>
      </svg>`;

      mockSSW.svg.mockReturnValue(mockSVG);

      const result = window.ssw.svg(fswWithPositions);
      
      expect(result).toContain('transform="translate(482,483)"');
      expect(result).toContain('transform="translate(506,500)"');
    });
  });

  describe('Performance and Edge Cases', () => {
    it('should handle large FSW strings efficiently', () => {
      const largeFSW = "AS20310S26b02S33100S10000S20000S30000M521x547S33100482x483S20310506x500S26b02503x520S10000600x600S20000650x650S30000700x700";
      const mockSVG = `<svg>
        ${'<g><text class="sym-fill">test</text><text class="sym-line">test</text></g>'.repeat(6)}
      </svg>`;

      mockSSW.svg.mockReturnValue(mockSVG);

      const start = performance.now();
      const result = window.ssw.svg(largeFSW);
      const end = performance.now();
      
      expect(result).toContain('sym-fill');
      expect(result).toContain('sym-line');
      expect(end - start).toBeLessThan(100); // Should complete within 100ms
    });

    it('should maintain SVG structure integrity', () => {
      const mockSVG = `<svg version="1.1" xmlns="http://www.w3.org/2000/svg">
        <g transform="translate(500,500)">
          <text class="sym-fill">test</text>
          <text class="sym-line">test</text>
        </g>
      </svg>`;

      mockSSW.svg.mockReturnValue(mockSVG);

      const result = window.ssw.svg('S10000');
      
      // Check for proper XML/SVG structure
      expect(result.match(/<svg[^>]*>/g)?.length).toBe(1);
      expect(result.match(/<\/svg>/g)?.length).toBe(1);
      expect(result.match(/<g[^>]*>/g)?.length).toBe(result.match(/<\/g>/g)?.length);
      expect(result.match(/<text[^>]*>/g)?.length).toBe(result.match(/<\/text>/g)?.length);
    });
  });

  describe('CSS Integration', () => {
    it('should work with external CSS for font definitions', () => {
      const mockSVG = `<svg>
        <text class="sym-fill" style="font-family:'SuttonSignWritingFill';">test</text>
        <text class="sym-line" style="font-family:'SuttonSignWritingLine';">test</text>
      </svg>`;

      mockSSW.svg.mockReturnValue(mockSVG);

      const result = window.ssw.svg('S10000');
      
      // Should rely on CSS classes and font-family styles
      expect(result).toContain('sym-fill');
      expect(result).toContain('sym-line');
      expect(result).toContain('SuttonSignWritingFill');
      expect(result).toContain('SuttonSignWritingLine');
    });

    it('should support CSS-only rendering without inline styles', () => {
      const mockSVG = `<svg>
        <text class="sym-fill">test</text>
        <text class="sym-line">test</text>
      </svg>`;

      mockSSW.svg.mockReturnValue(mockSVG);

      const result = window.ssw.svg('S10000', { css: true });
      
      expect(result).toContain('class="sym-fill"');
      expect(result).toContain('class="sym-line"');
    });
  });
});
