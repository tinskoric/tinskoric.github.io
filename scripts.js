document.addEventListener('DOMContentLoaded', () => {
    const toggleSwitch = document.querySelector('.style-toggle');
    const body = document.querySelector('body');

    const updateToggleText = () => {
        if (body.classList.contains('dark-mode')) {
            toggleSwitch.textContent = 'Dark Mode';
        } else {
            toggleSwitch.textContent = 'Light Mode';
        }
    };

    toggleSwitch.addEventListener('click', () => {
        if (body.classList.contains('light-mode')) {
            body.classList.remove('light-mode');
            body.classList.add('dark-mode');
        } else {
            body.classList.remove('dark-mode');
            body.classList.add('light-mode');
        }
        localStorage.setItem('mode', body.classList.contains('dark-mode') ? 'dark' : 'light');
        updateToggleText();
    });

    const savedMode = localStorage.getItem('mode');
    if (savedMode) {
        body.classList.add(savedMode === 'dark' ? 'dark-mode' : 'light-mode');
    } else {
        body.classList.add('light-mode');
    }
    updateToggleText();

    const cans = document.querySelectorAll('.can, .massive-can');

    const closeExpandedCan = () => {
        const expandedCan = document.querySelector('.expanded');
        if (expandedCan) {
            expandedCan.classList.remove('expanded');
            Object.assign(expandedCan.style, expandedCan.originalStyle);

            if (expandedCan.classList.contains('massive-can')) {
                expandedCan.querySelector('.body').style.maxHeight = '340px';
            } else {
                expandedCan.querySelector('.body').style.maxHeight = '100px';
            }

            expandedCan.querySelector('.body').style.overflow = 'hidden';
            expandedCan.style.cursor = 'zoom-in';
        }
    };

    cans.forEach(can => {
        can.addEventListener('click', () => {
            const rect = can.getBoundingClientRect();
            const scrollTop = window.pageYOffset || document.documentElement.scrollTop;
            const scrollLeft = window.pageXOffset || document.documentElement.scrollLeft;
            const top = rect.top + scrollTop;
            const left = rect.left + scrollLeft;

            if (can.classList.contains('expanded')) {
                // Close the currently expanded Can
                Object.assign(can.style, can.originalStyle);
                can.classList.remove('expanded');
                can.style.cursor = 'zoom-in';

                if (can.classList.contains('massive-can')) {
                    can.querySelector('.body').style.maxHeight = '340px';
                } else {
                    can.querySelector('.body').style.maxHeight = '100px';
                }

                can.querySelector('.body').style.overflow = 'hidden';
            } else {
                // Close any other expanded Can first
                closeExpandedCan();

                // Expand the clicked Can
                can.originalStyle = {
                    position: can.style.position,
                    zIndex: can.style.zIndex,
                    width: can.style.width,
                    height: can.style.height,
                    top: can.style.top,
                    left: can.style.left,
                    transform: can.style.transform,
                    transition: can.style.transition
                };

                can.style.position = 'fixed';
                can.style.zIndex = '1000';
                can.style.width = '80%';
                can.style.top = '50%';
                can.style.left = '50%';
                can.style.transform = 'translate(-50%, -50%)';
                can.style.transition = 'all 0.3s ease';
                can.classList.add('expanded');
                can.style.cursor = 'zoom-out';

                const body = can.querySelector('.body');
                can.style.height = 'auto';
                body.style.overflow = 'auto';
                body.style.maxHeight = 'none';
            }
        });
    });

    // Ensure all cans are non-expanded on page reload
    closeExpandedCan();
});

function copyLink() {
    navigator.clipboard.writeText(window.location.href);
    const notification = document.createElement('div');
    notification.textContent = "Link copied to clipboard";
    notification.className = 'notification';
    document.body.appendChild(notification);
    setTimeout(() => {
        document.body.removeChild(notification);
    }, 2000);
}
