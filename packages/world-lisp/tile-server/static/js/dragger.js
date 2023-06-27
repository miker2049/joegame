let pos = { top: 0, left: 0, x: 0, y: 0 };
const container = "container";
const scrollDiv = document.getElementById(container); // replace 'your_div_id' with the actual id of the div you want to scroll/pan
const mouseDownHandler = function (e) {
    pos = {
        // The current scroll
        left: scrollDiv.scrollLeft,
        top: scrollDiv.scrollTop,
        // Get the current mouse position
        x: e.clientX,
        y: e.clientY,
    };

    document.addEventListener("mousemove", mouseMoveHandler);
    document.addEventListener("mouseup", mouseUpHandler);
};

const mouseMoveHandler = function (e) {
    scrollDiv.style.cursor = "grabbing";
    scrollDiv.style.userSelect = "none";
    // How far the mouse has been moved
    const dx = e.clientX - pos.x;
    const dy = e.clientY - pos.y;
    // Scroll the element
    scrollDiv.scrollTop = pos.top - dy;
    scrollDiv.scrollLeft = pos.left - dx;
};

const mouseUpHandler = function () {
    document.removeEventListener("mousemove", mouseMoveHandler);
    document.removeEventListener("mouseup", mouseUpHandler);

    scrollDiv.style.cursor = "grab";
    scrollDiv.style.removeProperty("user-select");
};

document.addEventListener("mousedown", mouseDownHandler);

window.addEventListener("keydown", (event) => {
    switch (event.key) {
        case "ArrowLeft":
            scrollDiv.scrollBy({ left: -10 });
            break;
        case "ArrowRight":
            scrollDiv.scrollBy({ left: 10 });
            break;
        case "ArrowUp":
            scrollDiv.scrollBy({ top: -10 });
            break;
        case "ArrowDown":
            scrollDiv.scrollBy({ top: 10 });
            break;
        default:
            return;
    }
    event.preventDefault();
});
