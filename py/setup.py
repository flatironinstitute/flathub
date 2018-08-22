import setuptools

setuptools.setup(
    name="fi_astrosims",
    version="0.1",
    author="Dylan Simon, Flatiron Institute",
    author_email="dsimon@flatironinstitute.org",
    description="Client library for astrosims.flatironinstitute.org simulation data",
    url="https://github.com/flatironinstitute/astrosims-reproto",
    packages=setuptools.find_packages(),
    classifiers=(
        "Programming Language :: Python :: 2",
        "Programming Language :: Python :: 3",
	"License :: OSI Approved :: Apache Software License",
        "Operating System :: OS Independent",
    ), 
    install_requires=['numpy']
)
