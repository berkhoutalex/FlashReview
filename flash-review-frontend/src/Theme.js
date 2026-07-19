export const initThemeImpl = () => {
  const saved = window.localStorage.getItem("flashreview-theme");
  const theme = saved === "light" ? "light" : "dark";
  document.documentElement.setAttribute("data-theme", theme);
  return theme;
};

export const setThemeImpl = (theme) => () => {
  document.documentElement.setAttribute("data-theme", theme);
  window.localStorage.setItem("flashreview-theme", theme);
};
