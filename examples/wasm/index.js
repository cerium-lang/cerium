(async () => {
    /** @type {CanvasRenderingContext2D} */
    const ctx = document.getElementById("canvas").getContext("2d");

    ctx.canvas.width = innerWidth;
    ctx.canvas.height = innerHeight;

    ctx.fillStyle = "#000000";
    ctx.fillRect(0, 0, ctx.canvas.width, ctx.canvas.height);

    const wasm = await WebAssembly.instantiateStreaming(fetch("index.wasm"), {
        env: {
            get_width: () => ctx.canvas.width,
            get_height: () => ctx.canvas.width,
            set_color: (r, g, b) => (ctx.fillStyle = `rgb(${r}, ${g}, ${b})`),
            draw_point: (x, y) => ctx.fillRect(x, y, 2, 2),
            rand: (end) => Math.floor(Math.random() * end),
        },
    });

    const { setup, draw } = wasm.instance.exports;

    setup();

    requestAnimationFrame(loop = () => {
        draw();

        requestAnimationFrame(loop);
    });
})();
