export const classNames = function (...classes: string[]) {
  return classes.filter(Boolean).join(' ')
};