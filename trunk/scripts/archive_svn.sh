for project in olegmisc mathbot wp10bot; do

  echo "Fetching and archiving $project";
  svn checkout http://$project.googlecode.com/svn/trunk/ $project --username oleg.alexandrov;
  tar czfv $project.tgz $project;
  rm -rfv $project;
 
done;

