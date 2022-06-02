import React from 'react';
import { createRoot } from 'react-dom/client';
import { Provider } from 'react-redux';
import { store } from './app/store';
import App from './App';
import reportWebVitals from './reportWebVitals';
import './index.css';

const container = document.getElementById('root')!;
const root = createRoot(container);

root.render(
  <React.StrictMode>
    <Provider store={store}>
      <App />
      {/* TODO: THIS VERSION OF REACT ROUTER */}
    </Provider>
  </React.StrictMode>
);

// TODO: Update with GA-integration. See: https://nandolinhares.medium.com/google-analytics-with-react-and-typescript-june-2021-12d6f48f4842
// <!-- Global site tag (gtag.js) - Google Analytics: linked to elovero@flatironinstitute.org-->
// <script async src="https://www.googletagmanager.com/gtag/js?id=G-SFZ35MZK8H"></script>
// <script>
//   window.dataLayer = window.dataLayer || [];
//   function gtag(){dataLayer.push(arguments);}
//   gtag('js', new Date());
//   gtag('config', 'G-SFZ35MZK8H');
// </script>
// reportWebVitals(console.log);
