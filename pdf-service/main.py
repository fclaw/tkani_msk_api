from fastapi import FastAPI, Form, Response, HTTPException
from playwright.async_api import async_playwright
import uvicorn

app = FastAPI()

@app.post("/convert")
async def convert(
    text: str = Form(...),  # Raw HTML string
    input_format: str = Form("html"),
    content_viewport_width: str = Form(None),
    page_size: str = Form("A4"),
    orientation: str = Form("portrait")
):
    async with async_playwright() as p:
        # 1. Launch Browser
        browser = await p.chromium.launch(args=["--no-sandbox"])
        
        # 2. Mimic "content_viewport_width"
        # "balanced" in PDFCrowd usually means ~1200px to ensure tables don't squash
        viewport_width = 1280
        if content_viewport_width and content_viewport_width != "balanced":
            try:
                viewport_width = int(content_viewport_width)
            except ValueError:
                viewport_width = 1280

        context = await browser.new_context(
            viewport={'width': viewport_width, 'height': 800}
        )
        page = await context.new_page()

        try:
            # 3. Set Content
            await page.set_content(text, wait_until="networkidle")

            # 4. Map Orientation
            is_landscape = True if orientation.lower() == "landscape" else False

            # 5. Generate PDF
            pdf_bytes = await page.pdf(
                format=page_size,
                landscape=is_landscape,
                print_background=True,
                prefer_css_page_size=False
            )
            
            return Response(
                content=pdf_bytes,
                media_type="application/pdf"
            )

        except Exception as e:
            raise HTTPException(status_code=500, detail=str(e))
        finally:
            await browser.close()

if __name__ == "__main__":
    uvicorn.run(app, host="0.0.0.0", port=8000)