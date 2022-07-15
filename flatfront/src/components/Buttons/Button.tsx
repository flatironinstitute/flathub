import React from 'react';

interface ButtonProps {
  children?: React.ReactNode; // make the component able to receive children elements
  onClick?: (event: React.MouseEvent<HTMLButtonElement>) => void;
  variant?: 'primary' | 'secondary' | 'danger';
  size?: 'sm' | 'md' | 'lg';
  disabled?: boolean;
  className?: string; //for any additional/custom classes, try not to use!
}

const classes = {
  base: 'border border-transparent shadow-sm focus:outline-none transition ease-in-out duration-300 inline-flex items-center justify-center',
  disabled: 'opacity-50 cursor-not-allowed',
  size: {
    sm: 'px-2 py-1 text-sm rounded-sm',
    md: 'px-4 py-2 text-base rounded-md',
    lg: 'px-8 py-3 text-lg rounded-lg',
  },
  variant: {
    primary:
      'bg-blue-500 hover:bg-blue-800 focus:ring-2 focus:ring-blue-500 focus:ring-opacity-50 text-white',
    secondary:
      'bg-gray-200 hover:bg-gray-800 focus:ring-2 focus:ring-gray-500 focus:ring-opacity-50 text-gray-900 hover:text-white',
    danger:
      'bg-red-500 hover:bg-red-800 focus:ring-2 focus:ring-red-500 focus:ring-opacity-50 text-white',
  },
};

export const mkClass = (input: string) =>
  input
    .replace(/\s+/gm, ' ')
    .split(' ')
    .filter((cond) => typeof cond === 'string')
    .join(' ')
    .trim();

export const Button: React.FC<ButtonProps> = ({
  children,
  onClick,
  variant = 'primary',
  size = 'md',
  disabled = false,
  className = '',
  ...rest
}) => {
  return (
    <button
      className={mkClass(`
        ${classes.base}
        ${classes.size[size]}
        ${classes.variant[variant]}
        ${className}
      `)}
      onClick={onClick}
      disabled={disabled}
      {...rest}
    >
      {children}
    </button>
  );
};
