import * as React from 'react';
import { Routes, Route } from 'react-router-dom';
import useAllCatalogsService from './services/useAllCatalogsService';
const Home = React.lazy(() => import('./pages/Home'));
const Catalogs = React.lazy(() => import('./pages/Catalogs'));
const Collections = React.lazy(() => import('./pages/Collections'));
const About = React.lazy(() => import('./pages/About'));
const Loading = () => <p>Loading ...</p>;

const Main = () => {
  const service: {
    status: string;
    payload?: any;
  } = useAllCatalogsService();

  return (
    <React.Suspense fallback={<Loading />}>
      <Routes>
        <Route path="/" element={<Home />} />
        <Route
          path="/catalogs"
          element={<Catalogs catalogs={service.payload} />}
        />
        <Route path="/collections" element={<Collections />} />
        <Route path="/about" element={<About />} />
      </Routes>
    </React.Suspense>
  );
};
export default Main;
