import * as React from 'react';
import { Routes, Route } from 'react-router-dom';
import useAllCatalogsService from './services/useAllCatalogsService';
import About from './pages/About';
const Home = React.lazy(() => import('./pages/Home'));
const Catalogs = React.lazy(() => import('./pages/Catalogs'));
const Collections = React.lazy(() => import('./pages/Collections'));
const Loading = () => <p>Loading ...</p>;

const Main = () => {
  const catalogs: {
    status: string;
    payload?: any;
  } = useAllCatalogsService();

  return (
    <React.Suspense fallback={<Loading />}>
      <Routes>
        <Route path="/" element={<Home />} />
        <Route
          path="/catalogs"
          element={<Catalogs catalogs={catalogs.payload} />}
        />
        <Route path="/collections" element={<Collections />} />
        <Route path="/about" element={<About />} />
      </Routes>
    </React.Suspense>
  );
};
export default Main;
