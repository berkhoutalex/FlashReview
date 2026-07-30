"use strict";

const KEY = "frToken";

export const setTokenImpl = (token) => () => {
  window.localStorage.setItem(KEY, token);
};

export const getTokenImpl = (nothing) => (just) => () => {
  const token = window.localStorage.getItem(KEY);
  return token === null || token === "" ? nothing : just(token);
};

export const clearTokenImpl = () => {
  window.localStorage.removeItem(KEY);
};
