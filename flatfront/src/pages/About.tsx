const About = () => {
  return (
    <div className="mt-16 mx-auto max-w-7xl px-4 sm:mt-24 sm:px-6">
      <div className="text-center">
        <h1 className="text-4xl tracking-tight font-extrabold text-gray-900 sm:text-5xl md:text-6xl">
          <span className="block">About</span>
        </h1>
        <p className="mt-3 max-w-md mx-auto text-base text-gray-500 sm:text-lg md:mt-5 md:text-xl md:max-w-3xl">
          <b>FlatHUB</b> is a repository that enables researchers to easily
          explore and compare large astrophysics simulation datasets. Users can
          filter on multiple relevant fields, generate figures, and export their
          filtered data in a variety of formats. This project aims to facilitate
          discourse on the presented simulations by offering a web-based
          platform for exploration and analysis.
        </p>
      </div>
    </div>
  );
};
export default About;
