import * as React from 'react';
import { Routes, Route } from 'react-router-dom';
const Home = React.lazy(() => import('./components/Home'));
const Catalogs = React.lazy(() => import('./components/Catalogs'));
const Collections = React.lazy(() => import('./components/Collections'));
const About = React.lazy(() => import('./components/About'));
const Loading = () => <p>Loading ...</p>;

const Main = () => {
  return (
    <React.Suspense fallback={<Loading />}>
      <Routes>
        <Route path="/" element={<Home />} />
        <Route path="/catalogs" element={<Catalogs />} />
        <Route path="/collections" element={<Collections />} />
        <Route path="/about" element={<About />} />
      </Routes>
    </React.Suspense>
  );
};
export default Main;
